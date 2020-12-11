{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender audit suppression.
module Network.AWS.IoT.CreateAuditSuppression
  ( -- * Creating a request
    CreateAuditSuppression (..),
    mkCreateAuditSuppression,

    -- ** Request lenses
    casExpirationDate,
    casSuppressIndefinitely,
    casDescription,
    casCheckName,
    casResourceIdentifier,
    casClientRequestToken,

    -- * Destructuring the response
    CreateAuditSuppressionResponse (..),
    mkCreateAuditSuppressionResponse,

    -- ** Response lenses
    casrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAuditSuppression' smart constructor.
data CreateAuditSuppression = CreateAuditSuppression'
  { expirationDate ::
      Lude.Maybe Lude.Timestamp,
    suppressIndefinitely :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    checkName :: Lude.Text,
    resourceIdentifier :: ResourceIdentifier,
    clientRequestToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAuditSuppression' with the minimum fields required to make a request.
--
-- * 'checkName' - Undocumented field.
-- * 'clientRequestToken' - The epoch timestamp in seconds at which this suppression expires.
-- * 'description' - The description of the audit suppression.
-- * 'expirationDate' - The epoch timestamp in seconds at which this suppression expires.
-- * 'resourceIdentifier' - Undocumented field.
-- * 'suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
mkCreateAuditSuppression ::
  -- | 'checkName'
  Lude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  -- | 'clientRequestToken'
  Lude.Text ->
  CreateAuditSuppression
mkCreateAuditSuppression
  pCheckName_
  pResourceIdentifier_
  pClientRequestToken_ =
    CreateAuditSuppression'
      { expirationDate = Lude.Nothing,
        suppressIndefinitely = Lude.Nothing,
        description = Lude.Nothing,
        checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_,
        clientRequestToken = pClientRequestToken_
      }

-- | The epoch timestamp in seconds at which this suppression expires.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casExpirationDate :: Lens.Lens' CreateAuditSuppression (Lude.Maybe Lude.Timestamp)
casExpirationDate = Lens.lens (expirationDate :: CreateAuditSuppression -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: CreateAuditSuppression)
{-# DEPRECATED casExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Indicates whether a suppression should exist indefinitely or not.
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casSuppressIndefinitely :: Lens.Lens' CreateAuditSuppression (Lude.Maybe Lude.Bool)
casSuppressIndefinitely = Lens.lens (suppressIndefinitely :: CreateAuditSuppression -> Lude.Maybe Lude.Bool) (\s a -> s {suppressIndefinitely = a} :: CreateAuditSuppression)
{-# DEPRECATED casSuppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead." #-}

-- | The description of the audit suppression.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casDescription :: Lens.Lens' CreateAuditSuppression (Lude.Maybe Lude.Text)
casDescription = Lens.lens (description :: CreateAuditSuppression -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateAuditSuppression)
{-# DEPRECATED casDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casCheckName :: Lens.Lens' CreateAuditSuppression Lude.Text
casCheckName = Lens.lens (checkName :: CreateAuditSuppression -> Lude.Text) (\s a -> s {checkName = a} :: CreateAuditSuppression)
{-# DEPRECATED casCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casResourceIdentifier :: Lens.Lens' CreateAuditSuppression ResourceIdentifier
casResourceIdentifier = Lens.lens (resourceIdentifier :: CreateAuditSuppression -> ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: CreateAuditSuppression)
{-# DEPRECATED casResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The epoch timestamp in seconds at which this suppression expires.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casClientRequestToken :: Lens.Lens' CreateAuditSuppression Lude.Text
casClientRequestToken = Lens.lens (clientRequestToken :: CreateAuditSuppression -> Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateAuditSuppression)
{-# DEPRECATED casClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest CreateAuditSuppression where
  type Rs CreateAuditSuppression = CreateAuditSuppressionResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateAuditSuppressionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAuditSuppression where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateAuditSuppression where
  toJSON CreateAuditSuppression' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expirationDate" Lude..=) Lude.<$> expirationDate,
            ("suppressIndefinitely" Lude..=) Lude.<$> suppressIndefinitely,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("checkName" Lude..= checkName),
            Lude.Just ("resourceIdentifier" Lude..= resourceIdentifier),
            Lude.Just ("clientRequestToken" Lude..= clientRequestToken)
          ]
      )

instance Lude.ToPath CreateAuditSuppression where
  toPath = Lude.const "/audit/suppressions/create"

instance Lude.ToQuery CreateAuditSuppression where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAuditSuppressionResponse' smart constructor.
newtype CreateAuditSuppressionResponse = CreateAuditSuppressionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateAuditSuppressionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAuditSuppressionResponse
mkCreateAuditSuppressionResponse pResponseStatus_ =
  CreateAuditSuppressionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casrsResponseStatus :: Lens.Lens' CreateAuditSuppressionResponse Lude.Int
casrsResponseStatus = Lens.lens (responseStatus :: CreateAuditSuppressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAuditSuppressionResponse)
{-# DEPRECATED casrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
