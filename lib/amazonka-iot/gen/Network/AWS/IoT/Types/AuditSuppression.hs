{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditSuppression
  ( AuditSuppression (..),

    -- * Smart constructor
    mkAuditSuppression,

    -- * Lenses
    asExpirationDate,
    asSuppressIndefinitely,
    asDescription,
    asCheckName,
    asResourceIdentifier,
  )
where

import Network.AWS.IoT.Types.ResourceIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters out specific findings of a Device Defender audit.
--
-- /See:/ 'mkAuditSuppression' smart constructor.
data AuditSuppression = AuditSuppression'
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

-- | Creates a value of 'AuditSuppression' with the minimum fields required to make a request.
--
-- * 'checkName' - Undocumented field.
-- * 'description' - The description of the audit suppression.
-- * 'expirationDate' - The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
-- * 'resourceIdentifier' - Undocumented field.
-- * 'suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
mkAuditSuppression ::
  -- | 'checkName'
  Lude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  AuditSuppression
mkAuditSuppression pCheckName_ pResourceIdentifier_ =
  AuditSuppression'
    { expirationDate = Lude.Nothing,
      suppressIndefinitely = Lude.Nothing,
      description = Lude.Nothing,
      checkName = pCheckName_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asExpirationDate :: Lens.Lens' AuditSuppression (Lude.Maybe Lude.Timestamp)
asExpirationDate = Lens.lens (expirationDate :: AuditSuppression -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: AuditSuppression)
{-# DEPRECATED asExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Indicates whether a suppression should exist indefinitely or not.
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSuppressIndefinitely :: Lens.Lens' AuditSuppression (Lude.Maybe Lude.Bool)
asSuppressIndefinitely = Lens.lens (suppressIndefinitely :: AuditSuppression -> Lude.Maybe Lude.Bool) (\s a -> s {suppressIndefinitely = a} :: AuditSuppression)
{-# DEPRECATED asSuppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead." #-}

-- | The description of the audit suppression.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDescription :: Lens.Lens' AuditSuppression (Lude.Maybe Lude.Text)
asDescription = Lens.lens (description :: AuditSuppression -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AuditSuppression)
{-# DEPRECATED asDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCheckName :: Lens.Lens' AuditSuppression Lude.Text
asCheckName = Lens.lens (checkName :: AuditSuppression -> Lude.Text) (\s a -> s {checkName = a} :: AuditSuppression)
{-# DEPRECATED asCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asResourceIdentifier :: Lens.Lens' AuditSuppression ResourceIdentifier
asResourceIdentifier = Lens.lens (resourceIdentifier :: AuditSuppression -> ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: AuditSuppression)
{-# DEPRECATED asResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.FromJSON AuditSuppression where
  parseJSON =
    Lude.withObject
      "AuditSuppression"
      ( \x ->
          AuditSuppression'
            Lude.<$> (x Lude..:? "expirationDate")
            Lude.<*> (x Lude..:? "suppressIndefinitely")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..: "checkName")
            Lude.<*> (x Lude..: "resourceIdentifier")
      )
