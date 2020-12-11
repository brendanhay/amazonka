-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAConfigType
  ( SoftwareTokenMFAConfigType (..),

    -- * Smart constructor
    mkSoftwareTokenMFAConfigType,

    -- * Lenses
    stmctEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The type used for enabling software token MFA at the user pool level.
--
-- /See:/ 'mkSoftwareTokenMFAConfigType' smart constructor.
newtype SoftwareTokenMFAConfigType = SoftwareTokenMFAConfigType'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SoftwareTokenMFAConfigType' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether software token MFA is enabled.
mkSoftwareTokenMFAConfigType ::
  SoftwareTokenMFAConfigType
mkSoftwareTokenMFAConfigType =
  SoftwareTokenMFAConfigType' {enabled = Lude.Nothing}

-- | Specifies whether software token MFA is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmctEnabled :: Lens.Lens' SoftwareTokenMFAConfigType (Lude.Maybe Lude.Bool)
stmctEnabled = Lens.lens (enabled :: SoftwareTokenMFAConfigType -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SoftwareTokenMFAConfigType)
{-# DEPRECATED stmctEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromJSON SoftwareTokenMFAConfigType where
  parseJSON =
    Lude.withObject
      "SoftwareTokenMFAConfigType"
      ( \x ->
          SoftwareTokenMFAConfigType' Lude.<$> (x Lude..:? "Enabled")
      )

instance Lude.ToJSON SoftwareTokenMFAConfigType where
  toJSON SoftwareTokenMFAConfigType' {..} =
    Lude.object
      (Lude.catMaybes [("Enabled" Lude..=) Lude.<$> enabled])
