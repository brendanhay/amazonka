{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutAggregationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the aggregator account and region to collect data from the source account and region.
module Network.AWS.Config.PutAggregationAuthorization
  ( -- * Creating a request
    PutAggregationAuthorization (..),
    mkPutAggregationAuthorization,

    -- ** Request lenses
    paaTags,
    paaAuthorizedAccountId,
    paaAuthorizedAWSRegion,

    -- * Destructuring the response
    PutAggregationAuthorizationResponse (..),
    mkPutAggregationAuthorizationResponse,

    -- ** Response lenses
    paarsAggregationAuthorization,
    paarsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAggregationAuthorization' smart constructor.
data PutAggregationAuthorization = PutAggregationAuthorization'
  { tags ::
      Lude.Maybe [Tag],
    authorizedAccountId :: Lude.Text,
    authorizedAWSRegion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAggregationAuthorization' with the minimum fields required to make a request.
--
-- * 'authorizedAWSRegion' - The region authorized to collect aggregated data.
-- * 'authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
-- * 'tags' - An array of tag object.
mkPutAggregationAuthorization ::
  -- | 'authorizedAccountId'
  Lude.Text ->
  -- | 'authorizedAWSRegion'
  Lude.Text ->
  PutAggregationAuthorization
mkPutAggregationAuthorization
  pAuthorizedAccountId_
  pAuthorizedAWSRegion_ =
    PutAggregationAuthorization'
      { tags = Lude.Nothing,
        authorizedAccountId = pAuthorizedAccountId_,
        authorizedAWSRegion = pAuthorizedAWSRegion_
      }

-- | An array of tag object.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paaTags :: Lens.Lens' PutAggregationAuthorization (Lude.Maybe [Tag])
paaTags = Lens.lens (tags :: PutAggregationAuthorization -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutAggregationAuthorization)
{-# DEPRECATED paaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The 12-digit account ID of the account authorized to aggregate data.
--
-- /Note:/ Consider using 'authorizedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paaAuthorizedAccountId :: Lens.Lens' PutAggregationAuthorization Lude.Text
paaAuthorizedAccountId = Lens.lens (authorizedAccountId :: PutAggregationAuthorization -> Lude.Text) (\s a -> s {authorizedAccountId = a} :: PutAggregationAuthorization)
{-# DEPRECATED paaAuthorizedAccountId "Use generic-lens or generic-optics with 'authorizedAccountId' instead." #-}

-- | The region authorized to collect aggregated data.
--
-- /Note:/ Consider using 'authorizedAWSRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paaAuthorizedAWSRegion :: Lens.Lens' PutAggregationAuthorization Lude.Text
paaAuthorizedAWSRegion = Lens.lens (authorizedAWSRegion :: PutAggregationAuthorization -> Lude.Text) (\s a -> s {authorizedAWSRegion = a} :: PutAggregationAuthorization)
{-# DEPRECATED paaAuthorizedAWSRegion "Use generic-lens or generic-optics with 'authorizedAWSRegion' instead." #-}

instance Lude.AWSRequest PutAggregationAuthorization where
  type
    Rs PutAggregationAuthorization =
      PutAggregationAuthorizationResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutAggregationAuthorizationResponse'
            Lude.<$> (x Lude..?> "AggregationAuthorization")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAggregationAuthorization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutAggregationAuthorization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAggregationAuthorization where
  toJSON PutAggregationAuthorization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("AuthorizedAccountId" Lude..= authorizedAccountId),
            Lude.Just ("AuthorizedAwsRegion" Lude..= authorizedAWSRegion)
          ]
      )

instance Lude.ToPath PutAggregationAuthorization where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAggregationAuthorization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAggregationAuthorizationResponse' smart constructor.
data PutAggregationAuthorizationResponse = PutAggregationAuthorizationResponse'
  { aggregationAuthorization ::
      Lude.Maybe
        AggregationAuthorization,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAggregationAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'aggregationAuthorization' - Returns an AggregationAuthorization object.
-- * 'responseStatus' - The response status code.
mkPutAggregationAuthorizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAggregationAuthorizationResponse
mkPutAggregationAuthorizationResponse pResponseStatus_ =
  PutAggregationAuthorizationResponse'
    { aggregationAuthorization =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns an AggregationAuthorization object.
--
-- /Note:/ Consider using 'aggregationAuthorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paarsAggregationAuthorization :: Lens.Lens' PutAggregationAuthorizationResponse (Lude.Maybe AggregationAuthorization)
paarsAggregationAuthorization = Lens.lens (aggregationAuthorization :: PutAggregationAuthorizationResponse -> Lude.Maybe AggregationAuthorization) (\s a -> s {aggregationAuthorization = a} :: PutAggregationAuthorizationResponse)
{-# DEPRECATED paarsAggregationAuthorization "Use generic-lens or generic-optics with 'aggregationAuthorization' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paarsResponseStatus :: Lens.Lens' PutAggregationAuthorizationResponse Lude.Int
paarsResponseStatus = Lens.lens (responseStatus :: PutAggregationAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAggregationAuthorizationResponse)
{-# DEPRECATED paarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
