{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified option group.
module Network.AWS.RDS.CopyOptionGroup
  ( -- * Creating a request
    CopyOptionGroup (..),
    mkCopyOptionGroup,

    -- ** Request lenses
    cSourceOptionGroupIdentifier,
    cTargetOptionGroupDescription,
    cTargetOptionGroupIdentifier,
    cTags,

    -- * Destructuring the response
    CopyOptionGroupResponse (..),
    mkCopyOptionGroupResponse,

    -- ** Response lenses
    cogrsOptionGroup,
    cogrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCopyOptionGroup' smart constructor.
data CopyOptionGroup = CopyOptionGroup'
  { -- | The identifier for the source option group.
    --
    -- Constraints:
    --
    --     * Must specify a valid option group.
    sourceOptionGroupIdentifier :: Lude.Text,
    -- | The description for the copied option group.
    targetOptionGroupDescription :: Lude.Text,
    -- | The identifier for the copied option group.
    --
    -- Constraints:
    --
    --     * Can't be null, empty, or blank
    --
    --
    --     * Must contain from 1 to 255 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-option-group@
    targetOptionGroupIdentifier :: Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyOptionGroup' with the minimum fields required to make a request.
--
-- * 'sourceOptionGroupIdentifier' - The identifier for the source option group.
--
-- Constraints:
--
--     * Must specify a valid option group.
--
--
-- * 'targetOptionGroupDescription' - The description for the copied option group.
-- * 'targetOptionGroupIdentifier' - The identifier for the copied option group.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-option-group@
-- * 'tags' -
mkCopyOptionGroup ::
  -- | 'sourceOptionGroupIdentifier'
  Lude.Text ->
  -- | 'targetOptionGroupDescription'
  Lude.Text ->
  -- | 'targetOptionGroupIdentifier'
  Lude.Text ->
  CopyOptionGroup
mkCopyOptionGroup
  pSourceOptionGroupIdentifier_
  pTargetOptionGroupDescription_
  pTargetOptionGroupIdentifier_ =
    CopyOptionGroup'
      { sourceOptionGroupIdentifier =
          pSourceOptionGroupIdentifier_,
        targetOptionGroupDescription = pTargetOptionGroupDescription_,
        targetOptionGroupIdentifier = pTargetOptionGroupIdentifier_,
        tags = Lude.Nothing
      }

-- | The identifier for the source option group.
--
-- Constraints:
--
--     * Must specify a valid option group.
--
--
--
-- /Note:/ Consider using 'sourceOptionGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Lude.Text
cSourceOptionGroupIdentifier = Lens.lens (sourceOptionGroupIdentifier :: CopyOptionGroup -> Lude.Text) (\s a -> s {sourceOptionGroupIdentifier = a} :: CopyOptionGroup)
{-# DEPRECATED cSourceOptionGroupIdentifier "Use generic-lens or generic-optics with 'sourceOptionGroupIdentifier' instead." #-}

-- | The description for the copied option group.
--
-- /Note:/ Consider using 'targetOptionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetOptionGroupDescription :: Lens.Lens' CopyOptionGroup Lude.Text
cTargetOptionGroupDescription = Lens.lens (targetOptionGroupDescription :: CopyOptionGroup -> Lude.Text) (\s a -> s {targetOptionGroupDescription = a} :: CopyOptionGroup)
{-# DEPRECATED cTargetOptionGroupDescription "Use generic-lens or generic-optics with 'targetOptionGroupDescription' instead." #-}

-- | The identifier for the copied option group.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-option-group@
--
-- /Note:/ Consider using 'targetOptionGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Lude.Text
cTargetOptionGroupIdentifier = Lens.lens (targetOptionGroupIdentifier :: CopyOptionGroup -> Lude.Text) (\s a -> s {targetOptionGroupIdentifier = a} :: CopyOptionGroup)
{-# DEPRECATED cTargetOptionGroupIdentifier "Use generic-lens or generic-optics with 'targetOptionGroupIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CopyOptionGroup (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: CopyOptionGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopyOptionGroup)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CopyOptionGroup where
  type Rs CopyOptionGroup = CopyOptionGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CopyOptionGroupResult"
      ( \s h x ->
          CopyOptionGroupResponse'
            Lude.<$> (x Lude..@? "OptionGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyOptionGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopyOptionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyOptionGroup where
  toQuery CopyOptionGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopyOptionGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SourceOptionGroupIdentifier" Lude.=: sourceOptionGroupIdentifier,
        "TargetOptionGroupDescription"
          Lude.=: targetOptionGroupDescription,
        "TargetOptionGroupIdentifier" Lude.=: targetOptionGroupIdentifier,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCopyOptionGroupResponse' smart constructor.
data CopyOptionGroupResponse = CopyOptionGroupResponse'
  { optionGroup :: Lude.Maybe OptionGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyOptionGroupResponse' with the minimum fields required to make a request.
--
-- * 'optionGroup' -
-- * 'responseStatus' - The response status code.
mkCopyOptionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyOptionGroupResponse
mkCopyOptionGroupResponse pResponseStatus_ =
  CopyOptionGroupResponse'
    { optionGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrsOptionGroup :: Lens.Lens' CopyOptionGroupResponse (Lude.Maybe OptionGroup)
cogrsOptionGroup = Lens.lens (optionGroup :: CopyOptionGroupResponse -> Lude.Maybe OptionGroup) (\s a -> s {optionGroup = a} :: CopyOptionGroupResponse)
{-# DEPRECATED cogrsOptionGroup "Use generic-lens or generic-optics with 'optionGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogrsResponseStatus :: Lens.Lens' CopyOptionGroupResponse Lude.Int
cogrsResponseStatus = Lens.lens (responseStatus :: CopyOptionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyOptionGroupResponse)
{-# DEPRECATED cogrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
