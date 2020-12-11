{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender audit suppression.
module Network.AWS.IoT.UpdateAuditSuppression
  ( -- * Creating a request
    UpdateAuditSuppression (..),
    mkUpdateAuditSuppression,

    -- ** Request lenses
    uasExpirationDate,
    uasSuppressIndefinitely,
    uasDescription,
    uasCheckName,
    uasResourceIdentifier,

    -- * Destructuring the response
    UpdateAuditSuppressionResponse (..),
    mkUpdateAuditSuppressionResponse,

    -- ** Response lenses
    uasrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAuditSuppression' smart constructor.
data UpdateAuditSuppression = UpdateAuditSuppression'
  { expirationDate ::
      Lude.Maybe Lude.Timestamp,
    suppressIndefinitely :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    checkName :: Lude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAuditSuppression' with the minimum fields required to make a request.
--
-- * 'checkName' - Undocumented field.
-- * 'description' - The description of the audit suppression.
-- * 'expirationDate' - The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
-- * 'resourceIdentifier' - Undocumented field.
-- * 'suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
mkUpdateAuditSuppression ::
  -- | 'checkName'
  Lude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  UpdateAuditSuppression
mkUpdateAuditSuppression pCheckName_ pResourceIdentifier_ =
  UpdateAuditSuppression'
    { expirationDate = Lude.Nothing,
      suppressIndefinitely = Lude.Nothing,
      description = Lude.Nothing,
      checkName = pCheckName_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasExpirationDate :: Lens.Lens' UpdateAuditSuppression (Lude.Maybe Lude.Timestamp)
uasExpirationDate = Lens.lens (expirationDate :: UpdateAuditSuppression -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: UpdateAuditSuppression)
{-# DEPRECATED uasExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Indicates whether a suppression should exist indefinitely or not.
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSuppressIndefinitely :: Lens.Lens' UpdateAuditSuppression (Lude.Maybe Lude.Bool)
uasSuppressIndefinitely = Lens.lens (suppressIndefinitely :: UpdateAuditSuppression -> Lude.Maybe Lude.Bool) (\s a -> s {suppressIndefinitely = a} :: UpdateAuditSuppression)
{-# DEPRECATED uasSuppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead." #-}

-- | The description of the audit suppression.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasDescription :: Lens.Lens' UpdateAuditSuppression (Lude.Maybe Lude.Text)
uasDescription = Lens.lens (description :: UpdateAuditSuppression -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateAuditSuppression)
{-# DEPRECATED uasDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasCheckName :: Lens.Lens' UpdateAuditSuppression Lude.Text
uasCheckName = Lens.lens (checkName :: UpdateAuditSuppression -> Lude.Text) (\s a -> s {checkName = a} :: UpdateAuditSuppression)
{-# DEPRECATED uasCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasResourceIdentifier :: Lens.Lens' UpdateAuditSuppression ResourceIdentifier
uasResourceIdentifier = Lens.lens (resourceIdentifier :: UpdateAuditSuppression -> ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: UpdateAuditSuppression)
{-# DEPRECATED uasResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.AWSRequest UpdateAuditSuppression where
  type Rs UpdateAuditSuppression = UpdateAuditSuppressionResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateAuditSuppressionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAuditSuppression where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateAuditSuppression where
  toJSON UpdateAuditSuppression' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expirationDate" Lude..=) Lude.<$> expirationDate,
            ("suppressIndefinitely" Lude..=) Lude.<$> suppressIndefinitely,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("checkName" Lude..= checkName),
            Lude.Just ("resourceIdentifier" Lude..= resourceIdentifier)
          ]
      )

instance Lude.ToPath UpdateAuditSuppression where
  toPath = Lude.const "/audit/suppressions/update"

instance Lude.ToQuery UpdateAuditSuppression where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAuditSuppressionResponse' smart constructor.
newtype UpdateAuditSuppressionResponse = UpdateAuditSuppressionResponse'
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

-- | Creates a value of 'UpdateAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateAuditSuppressionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAuditSuppressionResponse
mkUpdateAuditSuppressionResponse pResponseStatus_ =
  UpdateAuditSuppressionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsResponseStatus :: Lens.Lens' UpdateAuditSuppressionResponse Lude.Int
uasrsResponseStatus = Lens.lens (responseStatus :: UpdateAuditSuppressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAuditSuppressionResponse)
{-# DEPRECATED uasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
