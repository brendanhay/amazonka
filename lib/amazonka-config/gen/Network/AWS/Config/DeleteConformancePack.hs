{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified conformance pack and all the AWS Config rules, remediation actions, and all evaluation results within that conformance pack.
--
-- AWS Config sets the conformance pack to @DELETE_IN_PROGRESS@ until the deletion is complete. You cannot update a conformance pack while it is in this state.
module Network.AWS.Config.DeleteConformancePack
  ( -- * Creating a request
    DeleteConformancePack (..),
    mkDeleteConformancePack,

    -- ** Request lenses
    dcpConformancePackName,

    -- * Destructuring the response
    DeleteConformancePackResponse (..),
    mkDeleteConformancePackResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteConformancePack' smart constructor.
newtype DeleteConformancePack = DeleteConformancePack'
  { -- | Name of the conformance pack you want to delete.
    conformancePackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConformancePack' with the minimum fields required to make a request.
--
-- * 'conformancePackName' - Name of the conformance pack you want to delete.
mkDeleteConformancePack ::
  -- | 'conformancePackName'
  Lude.Text ->
  DeleteConformancePack
mkDeleteConformancePack pConformancePackName_ =
  DeleteConformancePack'
    { conformancePackName =
        pConformancePackName_
    }

-- | Name of the conformance pack you want to delete.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpConformancePackName :: Lens.Lens' DeleteConformancePack Lude.Text
dcpConformancePackName = Lens.lens (conformancePackName :: DeleteConformancePack -> Lude.Text) (\s a -> s {conformancePackName = a} :: DeleteConformancePack)
{-# DEPRECATED dcpConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

instance Lude.AWSRequest DeleteConformancePack where
  type Rs DeleteConformancePack = DeleteConformancePackResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteConformancePackResponse'

instance Lude.ToHeaders DeleteConformancePack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.DeleteConformancePack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConformancePack where
  toJSON DeleteConformancePack' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ConformancePackName" Lude..= conformancePackName)]
      )

instance Lude.ToPath DeleteConformancePack where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConformancePack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConformancePackResponse' smart constructor.
data DeleteConformancePackResponse = DeleteConformancePackResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConformancePackResponse' with the minimum fields required to make a request.
mkDeleteConformancePackResponse ::
  DeleteConformancePackResponse
mkDeleteConformancePackResponse = DeleteConformancePackResponse'
