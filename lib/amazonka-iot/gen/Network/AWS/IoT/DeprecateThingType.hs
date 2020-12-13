{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeprecateThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates a thing type. You can not associate new things with deprecated thing type.
module Network.AWS.IoT.DeprecateThingType
  ( -- * Creating a request
    DeprecateThingType (..),
    mkDeprecateThingType,

    -- ** Request lenses
    dttfUndoDeprecate,
    dttfThingTypeName,

    -- * Destructuring the response
    DeprecateThingTypeResponse (..),
    mkDeprecateThingTypeResponse,

    -- ** Response lenses
    dttgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeprecateThingType operation.
--
-- /See:/ 'mkDeprecateThingType' smart constructor.
data DeprecateThingType = DeprecateThingType'
  { -- | Whether to undeprecate a deprecated thing type. If __true__ , the thing type will not be deprecated anymore and you can associate it with things.
    undoDeprecate :: Lude.Maybe Lude.Bool,
    -- | The name of the thing type to deprecate.
    thingTypeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateThingType' with the minimum fields required to make a request.
--
-- * 'undoDeprecate' - Whether to undeprecate a deprecated thing type. If __true__ , the thing type will not be deprecated anymore and you can associate it with things.
-- * 'thingTypeName' - The name of the thing type to deprecate.
mkDeprecateThingType ::
  -- | 'thingTypeName'
  Lude.Text ->
  DeprecateThingType
mkDeprecateThingType pThingTypeName_ =
  DeprecateThingType'
    { undoDeprecate = Lude.Nothing,
      thingTypeName = pThingTypeName_
    }

-- | Whether to undeprecate a deprecated thing type. If __true__ , the thing type will not be deprecated anymore and you can associate it with things.
--
-- /Note:/ Consider using 'undoDeprecate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfUndoDeprecate :: Lens.Lens' DeprecateThingType (Lude.Maybe Lude.Bool)
dttfUndoDeprecate = Lens.lens (undoDeprecate :: DeprecateThingType -> Lude.Maybe Lude.Bool) (\s a -> s {undoDeprecate = a} :: DeprecateThingType)
{-# DEPRECATED dttfUndoDeprecate "Use generic-lens or generic-optics with 'undoDeprecate' instead." #-}

-- | The name of the thing type to deprecate.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfThingTypeName :: Lens.Lens' DeprecateThingType Lude.Text
dttfThingTypeName = Lens.lens (thingTypeName :: DeprecateThingType -> Lude.Text) (\s a -> s {thingTypeName = a} :: DeprecateThingType)
{-# DEPRECATED dttfThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Lude.AWSRequest DeprecateThingType where
  type Rs DeprecateThingType = DeprecateThingTypeResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeprecateThingTypeResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeprecateThingType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DeprecateThingType where
  toJSON DeprecateThingType' {..} =
    Lude.object
      (Lude.catMaybes [("undoDeprecate" Lude..=) Lude.<$> undoDeprecate])

instance Lude.ToPath DeprecateThingType where
  toPath DeprecateThingType' {..} =
    Lude.mconcat
      ["/thing-types/", Lude.toBS thingTypeName, "/deprecate"]

instance Lude.ToQuery DeprecateThingType where
  toQuery = Lude.const Lude.mempty

-- | The output for the DeprecateThingType operation.
--
-- /See:/ 'mkDeprecateThingTypeResponse' smart constructor.
newtype DeprecateThingTypeResponse = DeprecateThingTypeResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateThingTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeprecateThingTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeprecateThingTypeResponse
mkDeprecateThingTypeResponse pResponseStatus_ =
  DeprecateThingTypeResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttgrsResponseStatus :: Lens.Lens' DeprecateThingTypeResponse Lude.Int
dttgrsResponseStatus = Lens.lens (responseStatus :: DeprecateThingTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeprecateThingTypeResponse)
{-# DEPRECATED dttgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
