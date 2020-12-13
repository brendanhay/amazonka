{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new option group. You can create up to 20 option groups.
module Network.AWS.RDS.CreateOptionGroup
  ( -- * Creating a request
    CreateOptionGroup (..),
    mkCreateOptionGroup,

    -- ** Request lenses
    cogOptionGroupDescription,
    cogEngineName,
    cogMajorEngineVersion,
    cogOptionGroupName,
    cogTags,

    -- * Destructuring the response
    CreateOptionGroupResponse (..),
    mkCreateOptionGroupResponse,

    -- ** Response lenses
    crsOptionGroup,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateOptionGroup' smart constructor.
data CreateOptionGroup = CreateOptionGroup'
  { -- | The description of the option group.
    optionGroupDescription :: Lude.Text,
    -- | Specifies the name of the engine that this option group should be associated with.
    engineName :: Lude.Text,
    -- | Specifies the major version of the engine that this option group should be associated with.
    majorEngineVersion :: Lude.Text,
    -- | Specifies the name of the option group to be created.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @myoptiongroup@
    optionGroupName :: Lude.Text,
    -- | Tags to assign to the option group.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOptionGroup' with the minimum fields required to make a request.
--
-- * 'optionGroupDescription' - The description of the option group.
-- * 'engineName' - Specifies the name of the engine that this option group should be associated with.
-- * 'majorEngineVersion' - Specifies the major version of the engine that this option group should be associated with.
-- * 'optionGroupName' - Specifies the name of the option group to be created.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @myoptiongroup@
-- * 'tags' - Tags to assign to the option group.
mkCreateOptionGroup ::
  -- | 'optionGroupDescription'
  Lude.Text ->
  -- | 'engineName'
  Lude.Text ->
  -- | 'majorEngineVersion'
  Lude.Text ->
  -- | 'optionGroupName'
  Lude.Text ->
  CreateOptionGroup
mkCreateOptionGroup
  pOptionGroupDescription_
  pEngineName_
  pMajorEngineVersion_
  pOptionGroupName_ =
    CreateOptionGroup'
      { optionGroupDescription =
          pOptionGroupDescription_,
        engineName = pEngineName_,
        majorEngineVersion = pMajorEngineVersion_,
        optionGroupName = pOptionGroupName_,
        tags = Lude.Nothing
      }

-- | The description of the option group.
--
-- /Note:/ Consider using 'optionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogOptionGroupDescription :: Lens.Lens' CreateOptionGroup Lude.Text
cogOptionGroupDescription = Lens.lens (optionGroupDescription :: CreateOptionGroup -> Lude.Text) (\s a -> s {optionGroupDescription = a} :: CreateOptionGroup)
{-# DEPRECATED cogOptionGroupDescription "Use generic-lens or generic-optics with 'optionGroupDescription' instead." #-}

-- | Specifies the name of the engine that this option group should be associated with.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogEngineName :: Lens.Lens' CreateOptionGroup Lude.Text
cogEngineName = Lens.lens (engineName :: CreateOptionGroup -> Lude.Text) (\s a -> s {engineName = a} :: CreateOptionGroup)
{-# DEPRECATED cogEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | Specifies the major version of the engine that this option group should be associated with.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogMajorEngineVersion :: Lens.Lens' CreateOptionGroup Lude.Text
cogMajorEngineVersion = Lens.lens (majorEngineVersion :: CreateOptionGroup -> Lude.Text) (\s a -> s {majorEngineVersion = a} :: CreateOptionGroup)
{-# DEPRECATED cogMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | Specifies the name of the option group to be created.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @myoptiongroup@
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogOptionGroupName :: Lens.Lens' CreateOptionGroup Lude.Text
cogOptionGroupName = Lens.lens (optionGroupName :: CreateOptionGroup -> Lude.Text) (\s a -> s {optionGroupName = a} :: CreateOptionGroup)
{-# DEPRECATED cogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | Tags to assign to the option group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cogTags :: Lens.Lens' CreateOptionGroup (Lude.Maybe [Tag])
cogTags = Lens.lens (tags :: CreateOptionGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateOptionGroup)
{-# DEPRECATED cogTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateOptionGroup where
  type Rs CreateOptionGroup = CreateOptionGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateOptionGroupResult"
      ( \s h x ->
          CreateOptionGroupResponse'
            Lude.<$> (x Lude..@? "OptionGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOptionGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateOptionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateOptionGroup where
  toQuery CreateOptionGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateOptionGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "OptionGroupDescription" Lude.=: optionGroupDescription,
        "EngineName" Lude.=: engineName,
        "MajorEngineVersion" Lude.=: majorEngineVersion,
        "OptionGroupName" Lude.=: optionGroupName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateOptionGroupResponse' smart constructor.
data CreateOptionGroupResponse = CreateOptionGroupResponse'
  { optionGroup :: Lude.Maybe OptionGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOptionGroupResponse' with the minimum fields required to make a request.
--
-- * 'optionGroup' -
-- * 'responseStatus' - The response status code.
mkCreateOptionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOptionGroupResponse
mkCreateOptionGroupResponse pResponseStatus_ =
  CreateOptionGroupResponse'
    { optionGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsOptionGroup :: Lens.Lens' CreateOptionGroupResponse (Lude.Maybe OptionGroup)
crsOptionGroup = Lens.lens (optionGroup :: CreateOptionGroupResponse -> Lude.Maybe OptionGroup) (\s a -> s {optionGroup = a} :: CreateOptionGroupResponse)
{-# DEPRECATED crsOptionGroup "Use generic-lens or generic-optics with 'optionGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateOptionGroupResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateOptionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOptionGroupResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
