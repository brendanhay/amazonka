{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit suppression.
module Network.AWS.IoT.DescribeAuditSuppression
  ( -- * Creating a request
    DescribeAuditSuppression (..),
    mkDescribeAuditSuppression,

    -- ** Request lenses
    dCheckName,
    dResourceIdentifier,

    -- * Destructuring the response
    DescribeAuditSuppressionResponse (..),
    mkDescribeAuditSuppressionResponse,

    -- ** Response lenses
    dasfrsCheckName,
    dasfrsExpirationDate,
    dasfrsSuppressIndefinitely,
    dasfrsDescription,
    dasfrsResourceIdentifier,
    dasfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAuditSuppression' smart constructor.
data DescribeAuditSuppression = DescribeAuditSuppression'
  { checkName :: Lude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAuditSuppression' with the minimum fields required to make a request.
--
-- * 'checkName' -
-- * 'resourceIdentifier' -
mkDescribeAuditSuppression ::
  -- | 'checkName'
  Lude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  DescribeAuditSuppression
mkDescribeAuditSuppression pCheckName_ pResourceIdentifier_ =
  DescribeAuditSuppression'
    { checkName = pCheckName_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCheckName :: Lens.Lens' DescribeAuditSuppression Lude.Text
dCheckName = Lens.lens (checkName :: DescribeAuditSuppression -> Lude.Text) (\s a -> s {checkName = a} :: DescribeAuditSuppression)
{-# DEPRECATED dCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceIdentifier :: Lens.Lens' DescribeAuditSuppression ResourceIdentifier
dResourceIdentifier = Lens.lens (resourceIdentifier :: DescribeAuditSuppression -> ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: DescribeAuditSuppression)
{-# DEPRECATED dResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.AWSRequest DescribeAuditSuppression where
  type Rs DescribeAuditSuppression = DescribeAuditSuppressionResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAuditSuppressionResponse'
            Lude.<$> (x Lude..?> "checkName")
            Lude.<*> (x Lude..?> "expirationDate")
            Lude.<*> (x Lude..?> "suppressIndefinitely")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "resourceIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAuditSuppression where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DescribeAuditSuppression where
  toJSON DescribeAuditSuppression' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("checkName" Lude..= checkName),
            Lude.Just ("resourceIdentifier" Lude..= resourceIdentifier)
          ]
      )

instance Lude.ToPath DescribeAuditSuppression where
  toPath = Lude.const "/audit/suppressions/describe"

instance Lude.ToQuery DescribeAuditSuppression where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAuditSuppressionResponse' smart constructor.
data DescribeAuditSuppressionResponse = DescribeAuditSuppressionResponse'
  { checkName :: Lude.Maybe Lude.Text,
    -- | The epoch timestamp in seconds at which this suppression expires.
    expirationDate :: Lude.Maybe Lude.Timestamp,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Lude.Maybe Lude.Bool,
    -- | The description of the audit suppression.
    description :: Lude.Maybe Lude.Text,
    resourceIdentifier :: Lude.Maybe ResourceIdentifier,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- * 'checkName' -
-- * 'expirationDate' - The epoch timestamp in seconds at which this suppression expires.
-- * 'suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
-- * 'description' - The description of the audit suppression.
-- * 'resourceIdentifier' -
-- * 'responseStatus' - The response status code.
mkDescribeAuditSuppressionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAuditSuppressionResponse
mkDescribeAuditSuppressionResponse pResponseStatus_ =
  DescribeAuditSuppressionResponse'
    { checkName = Lude.Nothing,
      expirationDate = Lude.Nothing,
      suppressIndefinitely = Lude.Nothing,
      description = Lude.Nothing,
      resourceIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsCheckName :: Lens.Lens' DescribeAuditSuppressionResponse (Lude.Maybe Lude.Text)
dasfrsCheckName = Lens.lens (checkName :: DescribeAuditSuppressionResponse -> Lude.Maybe Lude.Text) (\s a -> s {checkName = a} :: DescribeAuditSuppressionResponse)
{-# DEPRECATED dasfrsCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | The epoch timestamp in seconds at which this suppression expires.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsExpirationDate :: Lens.Lens' DescribeAuditSuppressionResponse (Lude.Maybe Lude.Timestamp)
dasfrsExpirationDate = Lens.lens (expirationDate :: DescribeAuditSuppressionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: DescribeAuditSuppressionResponse)
{-# DEPRECATED dasfrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Indicates whether a suppression should exist indefinitely or not.
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsSuppressIndefinitely :: Lens.Lens' DescribeAuditSuppressionResponse (Lude.Maybe Lude.Bool)
dasfrsSuppressIndefinitely = Lens.lens (suppressIndefinitely :: DescribeAuditSuppressionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {suppressIndefinitely = a} :: DescribeAuditSuppressionResponse)
{-# DEPRECATED dasfrsSuppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead." #-}

-- | The description of the audit suppression.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsDescription :: Lens.Lens' DescribeAuditSuppressionResponse (Lude.Maybe Lude.Text)
dasfrsDescription = Lens.lens (description :: DescribeAuditSuppressionResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeAuditSuppressionResponse)
{-# DEPRECATED dasfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsResourceIdentifier :: Lens.Lens' DescribeAuditSuppressionResponse (Lude.Maybe ResourceIdentifier)
dasfrsResourceIdentifier = Lens.lens (resourceIdentifier :: DescribeAuditSuppressionResponse -> Lude.Maybe ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: DescribeAuditSuppressionResponse)
{-# DEPRECATED dasfrsResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsResponseStatus :: Lens.Lens' DescribeAuditSuppressionResponse Lude.Int
dasfrsResponseStatus = Lens.lens (responseStatus :: DescribeAuditSuppressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAuditSuppressionResponse)
{-# DEPRECATED dasfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
