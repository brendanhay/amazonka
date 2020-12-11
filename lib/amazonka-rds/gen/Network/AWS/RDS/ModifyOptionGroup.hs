{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing option group.
module Network.AWS.RDS.ModifyOptionGroup
  ( -- * Creating a request
    ModifyOptionGroup (..),
    mkModifyOptionGroup,

    -- ** Request lenses
    mogOptionsToInclude,
    mogOptionsToRemove,
    mogApplyImmediately,
    mogOptionGroupName,

    -- * Destructuring the response
    ModifyOptionGroupResponse (..),
    mkModifyOptionGroupResponse,

    -- ** Response lenses
    mogrsOptionGroup,
    mogrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyOptionGroup' smart constructor.
data ModifyOptionGroup = ModifyOptionGroup'
  { optionsToInclude ::
      Lude.Maybe [OptionConfiguration],
    optionsToRemove :: Lude.Maybe [Lude.Text],
    applyImmediately :: Lude.Maybe Lude.Bool,
    optionGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyOptionGroup' with the minimum fields required to make a request.
--
-- * 'applyImmediately' - A value that indicates whether to apply the change immediately or during the next maintenance window for each instance associated with the option group.
-- * 'optionGroupName' - The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
-- * 'optionsToInclude' - Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
-- * 'optionsToRemove' - Options in this list are removed from the option group.
mkModifyOptionGroup ::
  -- | 'optionGroupName'
  Lude.Text ->
  ModifyOptionGroup
mkModifyOptionGroup pOptionGroupName_ =
  ModifyOptionGroup'
    { optionsToInclude = Lude.Nothing,
      optionsToRemove = Lude.Nothing,
      applyImmediately = Lude.Nothing,
      optionGroupName = pOptionGroupName_
    }

-- | Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
--
-- /Note:/ Consider using 'optionsToInclude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionsToInclude :: Lens.Lens' ModifyOptionGroup (Lude.Maybe [OptionConfiguration])
mogOptionsToInclude = Lens.lens (optionsToInclude :: ModifyOptionGroup -> Lude.Maybe [OptionConfiguration]) (\s a -> s {optionsToInclude = a} :: ModifyOptionGroup)
{-# DEPRECATED mogOptionsToInclude "Use generic-lens or generic-optics with 'optionsToInclude' instead." #-}

-- | Options in this list are removed from the option group.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionsToRemove :: Lens.Lens' ModifyOptionGroup (Lude.Maybe [Lude.Text])
mogOptionsToRemove = Lens.lens (optionsToRemove :: ModifyOptionGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {optionsToRemove = a} :: ModifyOptionGroup)
{-# DEPRECATED mogOptionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead." #-}

-- | A value that indicates whether to apply the change immediately or during the next maintenance window for each instance associated with the option group.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogApplyImmediately :: Lens.Lens' ModifyOptionGroup (Lude.Maybe Lude.Bool)
mogApplyImmediately = Lens.lens (applyImmediately :: ModifyOptionGroup -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyOptionGroup)
{-# DEPRECATED mogApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogOptionGroupName :: Lens.Lens' ModifyOptionGroup Lude.Text
mogOptionGroupName = Lens.lens (optionGroupName :: ModifyOptionGroup -> Lude.Text) (\s a -> s {optionGroupName = a} :: ModifyOptionGroup)
{-# DEPRECATED mogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

instance Lude.AWSRequest ModifyOptionGroup where
  type Rs ModifyOptionGroup = ModifyOptionGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyOptionGroupResult"
      ( \s h x ->
          ModifyOptionGroupResponse'
            Lude.<$> (x Lude..@? "OptionGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyOptionGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyOptionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyOptionGroup where
  toQuery ModifyOptionGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyOptionGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "OptionsToInclude"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "OptionConfiguration" Lude.<$> optionsToInclude),
        "OptionsToRemove"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionsToRemove),
        "ApplyImmediately" Lude.=: applyImmediately,
        "OptionGroupName" Lude.=: optionGroupName
      ]

-- | /See:/ 'mkModifyOptionGroupResponse' smart constructor.
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
  { optionGroup ::
      Lude.Maybe OptionGroup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyOptionGroupResponse' with the minimum fields required to make a request.
--
-- * 'optionGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyOptionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyOptionGroupResponse
mkModifyOptionGroupResponse pResponseStatus_ =
  ModifyOptionGroupResponse'
    { optionGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogrsOptionGroup :: Lens.Lens' ModifyOptionGroupResponse (Lude.Maybe OptionGroup)
mogrsOptionGroup = Lens.lens (optionGroup :: ModifyOptionGroupResponse -> Lude.Maybe OptionGroup) (\s a -> s {optionGroup = a} :: ModifyOptionGroupResponse)
{-# DEPRECATED mogrsOptionGroup "Use generic-lens or generic-optics with 'optionGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mogrsResponseStatus :: Lens.Lens' ModifyOptionGroupResponse Lude.Int
mogrsResponseStatus = Lens.lens (responseStatus :: ModifyOptionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyOptionGroupResponse)
{-# DEPRECATED mogrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
