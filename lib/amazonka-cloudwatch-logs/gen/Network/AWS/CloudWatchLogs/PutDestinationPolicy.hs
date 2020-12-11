{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutDestinationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an access policy associated with an existing destination. An access policy is an <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies_overview.html IAM policy document> that is used to authorize claims to register a subscription filter against a given destination.
module Network.AWS.CloudWatchLogs.PutDestinationPolicy
  ( -- * Creating a request
    PutDestinationPolicy (..),
    mkPutDestinationPolicy,

    -- ** Request lenses
    pdpDestinationName,
    pdpAccessPolicy,

    -- * Destructuring the response
    PutDestinationPolicyResponse (..),
    mkPutDestinationPolicyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutDestinationPolicy' smart constructor.
data PutDestinationPolicy = PutDestinationPolicy'
  { destinationName ::
      Lude.Text,
    accessPolicy :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDestinationPolicy' with the minimum fields required to make a request.
--
-- * 'accessPolicy' - An IAM policy document that authorizes cross-account users to deliver their log events to the associated destination. This can be up to 5120 bytes.
-- * 'destinationName' - A name for an existing destination.
mkPutDestinationPolicy ::
  -- | 'destinationName'
  Lude.Text ->
  -- | 'accessPolicy'
  Lude.Text ->
  PutDestinationPolicy
mkPutDestinationPolicy pDestinationName_ pAccessPolicy_ =
  PutDestinationPolicy'
    { destinationName = pDestinationName_,
      accessPolicy = pAccessPolicy_
    }

-- | A name for an existing destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpDestinationName :: Lens.Lens' PutDestinationPolicy Lude.Text
pdpDestinationName = Lens.lens (destinationName :: PutDestinationPolicy -> Lude.Text) (\s a -> s {destinationName = a} :: PutDestinationPolicy)
{-# DEPRECATED pdpDestinationName "Use generic-lens or generic-optics with 'destinationName' instead." #-}

-- | An IAM policy document that authorizes cross-account users to deliver their log events to the associated destination. This can be up to 5120 bytes.
--
-- /Note:/ Consider using 'accessPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpAccessPolicy :: Lens.Lens' PutDestinationPolicy Lude.Text
pdpAccessPolicy = Lens.lens (accessPolicy :: PutDestinationPolicy -> Lude.Text) (\s a -> s {accessPolicy = a} :: PutDestinationPolicy)
{-# DEPRECATED pdpAccessPolicy "Use generic-lens or generic-optics with 'accessPolicy' instead." #-}

instance Lude.AWSRequest PutDestinationPolicy where
  type Rs PutDestinationPolicy = PutDestinationPolicyResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull PutDestinationPolicyResponse'

instance Lude.ToHeaders PutDestinationPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutDestinationPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutDestinationPolicy where
  toJSON PutDestinationPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("destinationName" Lude..= destinationName),
            Lude.Just ("accessPolicy" Lude..= accessPolicy)
          ]
      )

instance Lude.ToPath PutDestinationPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutDestinationPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutDestinationPolicyResponse' smart constructor.
data PutDestinationPolicyResponse = PutDestinationPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDestinationPolicyResponse' with the minimum fields required to make a request.
mkPutDestinationPolicyResponse ::
  PutDestinationPolicyResponse
mkPutDestinationPolicyResponse = PutDestinationPolicyResponse'
