{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserHierarchyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the user hierarchy structure: add, remove, and rename user hierarchy levels.
module Network.AWS.Connect.UpdateUserHierarchyStructure
  ( -- * Creating a request
    UpdateUserHierarchyStructure (..),
    mkUpdateUserHierarchyStructure,

    -- ** Request lenses
    uuhsInstanceId,
    uuhsHierarchyStructure,

    -- * Destructuring the response
    UpdateUserHierarchyStructureResponse (..),
    mkUpdateUserHierarchyStructureResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserHierarchyStructure' smart constructor.
data UpdateUserHierarchyStructure = UpdateUserHierarchyStructure'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The hierarchy levels to update.
    hierarchyStructure :: HierarchyStructureUpdate
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserHierarchyStructure' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'hierarchyStructure' - The hierarchy levels to update.
mkUpdateUserHierarchyStructure ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'hierarchyStructure'
  HierarchyStructureUpdate ->
  UpdateUserHierarchyStructure
mkUpdateUserHierarchyStructure pInstanceId_ pHierarchyStructure_ =
  UpdateUserHierarchyStructure'
    { instanceId = pInstanceId_,
      hierarchyStructure = pHierarchyStructure_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhsInstanceId :: Lens.Lens' UpdateUserHierarchyStructure Lude.Text
uuhsInstanceId = Lens.lens (instanceId :: UpdateUserHierarchyStructure -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateUserHierarchyStructure)
{-# DEPRECATED uuhsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The hierarchy levels to update.
--
-- /Note:/ Consider using 'hierarchyStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuhsHierarchyStructure :: Lens.Lens' UpdateUserHierarchyStructure HierarchyStructureUpdate
uuhsHierarchyStructure = Lens.lens (hierarchyStructure :: UpdateUserHierarchyStructure -> HierarchyStructureUpdate) (\s a -> s {hierarchyStructure = a} :: UpdateUserHierarchyStructure)
{-# DEPRECATED uuhsHierarchyStructure "Use generic-lens or generic-optics with 'hierarchyStructure' instead." #-}

instance Lude.AWSRequest UpdateUserHierarchyStructure where
  type
    Rs UpdateUserHierarchyStructure =
      UpdateUserHierarchyStructureResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateUserHierarchyStructureResponse'

instance Lude.ToHeaders UpdateUserHierarchyStructure where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserHierarchyStructure where
  toJSON UpdateUserHierarchyStructure' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("HierarchyStructure" Lude..= hierarchyStructure)]
      )

instance Lude.ToPath UpdateUserHierarchyStructure where
  toPath UpdateUserHierarchyStructure' {..} =
    Lude.mconcat ["/user-hierarchy-structure/", Lude.toBS instanceId]

instance Lude.ToQuery UpdateUserHierarchyStructure where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserHierarchyStructureResponse' smart constructor.
data UpdateUserHierarchyStructureResponse = UpdateUserHierarchyStructureResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserHierarchyStructureResponse' with the minimum fields required to make a request.
mkUpdateUserHierarchyStructureResponse ::
  UpdateUserHierarchyStructureResponse
mkUpdateUserHierarchyStructureResponse =
  UpdateUserHierarchyStructureResponse'
