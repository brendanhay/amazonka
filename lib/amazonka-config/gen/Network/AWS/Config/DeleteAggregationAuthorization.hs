{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteAggregationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the authorization granted to the specified configuration aggregator account in a specified region.
module Network.AWS.Config.DeleteAggregationAuthorization
  ( -- * Creating a request
    DeleteAggregationAuthorization (..),
    mkDeleteAggregationAuthorization,

    -- ** Request lenses
    daaAuthorizedAWSRegion,
    daaAuthorizedAccountId,

    -- * Destructuring the response
    DeleteAggregationAuthorizationResponse (..),
    mkDeleteAggregationAuthorizationResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAggregationAuthorization' smart constructor.
data DeleteAggregationAuthorization = DeleteAggregationAuthorization'
  { -- | The region authorized to collect aggregated data.
    authorizedAWSRegion :: Lude.Text,
    -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAggregationAuthorization' with the minimum fields required to make a request.
--
-- * 'authorizedAWSRegion' - The region authorized to collect aggregated data.
-- * 'authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
mkDeleteAggregationAuthorization ::
  -- | 'authorizedAWSRegion'
  Lude.Text ->
  -- | 'authorizedAccountId'
  Lude.Text ->
  DeleteAggregationAuthorization
mkDeleteAggregationAuthorization
  pAuthorizedAWSRegion_
  pAuthorizedAccountId_ =
    DeleteAggregationAuthorization'
      { authorizedAWSRegion =
          pAuthorizedAWSRegion_,
        authorizedAccountId = pAuthorizedAccountId_
      }

-- | The region authorized to collect aggregated data.
--
-- /Note:/ Consider using 'authorizedAWSRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAuthorizedAWSRegion :: Lens.Lens' DeleteAggregationAuthorization Lude.Text
daaAuthorizedAWSRegion = Lens.lens (authorizedAWSRegion :: DeleteAggregationAuthorization -> Lude.Text) (\s a -> s {authorizedAWSRegion = a} :: DeleteAggregationAuthorization)
{-# DEPRECATED daaAuthorizedAWSRegion "Use generic-lens or generic-optics with 'authorizedAWSRegion' instead." #-}

-- | The 12-digit account ID of the account authorized to aggregate data.
--
-- /Note:/ Consider using 'authorizedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAuthorizedAccountId :: Lens.Lens' DeleteAggregationAuthorization Lude.Text
daaAuthorizedAccountId = Lens.lens (authorizedAccountId :: DeleteAggregationAuthorization -> Lude.Text) (\s a -> s {authorizedAccountId = a} :: DeleteAggregationAuthorization)
{-# DEPRECATED daaAuthorizedAccountId "Use generic-lens or generic-optics with 'authorizedAccountId' instead." #-}

instance Lude.AWSRequest DeleteAggregationAuthorization where
  type
    Rs DeleteAggregationAuthorization =
      DeleteAggregationAuthorizationResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteAggregationAuthorizationResponse'

instance Lude.ToHeaders DeleteAggregationAuthorization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteAggregationAuthorization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAggregationAuthorization where
  toJSON DeleteAggregationAuthorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AuthorizedAwsRegion" Lude..= authorizedAWSRegion),
            Lude.Just ("AuthorizedAccountId" Lude..= authorizedAccountId)
          ]
      )

instance Lude.ToPath DeleteAggregationAuthorization where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAggregationAuthorization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAggregationAuthorizationResponse' smart constructor.
data DeleteAggregationAuthorizationResponse = DeleteAggregationAuthorizationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAggregationAuthorizationResponse' with the minimum fields required to make a request.
mkDeleteAggregationAuthorizationResponse ::
  DeleteAggregationAuthorizationResponse
mkDeleteAggregationAuthorizationResponse =
  DeleteAggregationAuthorizationResponse'
