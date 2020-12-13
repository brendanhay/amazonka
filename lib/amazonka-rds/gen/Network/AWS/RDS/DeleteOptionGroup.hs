{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing option group.
module Network.AWS.RDS.DeleteOptionGroup
  ( -- * Creating a request
    DeleteOptionGroup (..),
    mkDeleteOptionGroup,

    -- ** Request lenses
    dogOptionGroupName,

    -- * Destructuring the response
    DeleteOptionGroupResponse (..),
    mkDeleteOptionGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteOptionGroup' smart constructor.
newtype DeleteOptionGroup = DeleteOptionGroup'
  { -- | The name of the option group to be deleted.
    optionGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOptionGroup' with the minimum fields required to make a request.
--
-- * 'optionGroupName' - The name of the option group to be deleted.
mkDeleteOptionGroup ::
  -- | 'optionGroupName'
  Lude.Text ->
  DeleteOptionGroup
mkDeleteOptionGroup pOptionGroupName_ =
  DeleteOptionGroup' {optionGroupName = pOptionGroupName_}

-- | The name of the option group to be deleted.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogOptionGroupName :: Lens.Lens' DeleteOptionGroup Lude.Text
dogOptionGroupName = Lens.lens (optionGroupName :: DeleteOptionGroup -> Lude.Text) (\s a -> s {optionGroupName = a} :: DeleteOptionGroup)
{-# DEPRECATED dogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

instance Lude.AWSRequest DeleteOptionGroup where
  type Rs DeleteOptionGroup = DeleteOptionGroupResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull DeleteOptionGroupResponse'

instance Lude.ToHeaders DeleteOptionGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteOptionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOptionGroup where
  toQuery DeleteOptionGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteOptionGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "OptionGroupName" Lude.=: optionGroupName
      ]

-- | /See:/ 'mkDeleteOptionGroupResponse' smart constructor.
data DeleteOptionGroupResponse = DeleteOptionGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOptionGroupResponse' with the minimum fields required to make a request.
mkDeleteOptionGroupResponse ::
  DeleteOptionGroupResponse
mkDeleteOptionGroupResponse = DeleteOptionGroupResponse'
