{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.AliasListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.AliasListEntry
  ( AliasListEntry (..),

    -- * Smart constructor
    mkAliasListEntry,

    -- * Lenses
    aleTargetKeyId,
    aleAliasName,
    aleAliasARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an alias.
--
-- /See:/ 'mkAliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
  { -- | String that contains the key identifier referred to by the alias.
    targetKeyId :: Lude.Maybe Lude.Text,
    -- | String that contains the alias. This value begins with @alias/@ .
    aliasName :: Lude.Maybe Lude.Text,
    -- | String that contains the key ARN.
    aliasARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AliasListEntry' with the minimum fields required to make a request.
--
-- * 'targetKeyId' - String that contains the key identifier referred to by the alias.
-- * 'aliasName' - String that contains the alias. This value begins with @alias/@ .
-- * 'aliasARN' - String that contains the key ARN.
mkAliasListEntry ::
  AliasListEntry
mkAliasListEntry =
  AliasListEntry'
    { targetKeyId = Lude.Nothing,
      aliasName = Lude.Nothing,
      aliasARN = Lude.Nothing
    }

-- | String that contains the key identifier referred to by the alias.
--
-- /Note:/ Consider using 'targetKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aleTargetKeyId :: Lens.Lens' AliasListEntry (Lude.Maybe Lude.Text)
aleTargetKeyId = Lens.lens (targetKeyId :: AliasListEntry -> Lude.Maybe Lude.Text) (\s a -> s {targetKeyId = a} :: AliasListEntry)
{-# DEPRECATED aleTargetKeyId "Use generic-lens or generic-optics with 'targetKeyId' instead." #-}

-- | String that contains the alias. This value begins with @alias/@ .
--
-- /Note:/ Consider using 'aliasName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aleAliasName :: Lens.Lens' AliasListEntry (Lude.Maybe Lude.Text)
aleAliasName = Lens.lens (aliasName :: AliasListEntry -> Lude.Maybe Lude.Text) (\s a -> s {aliasName = a} :: AliasListEntry)
{-# DEPRECATED aleAliasName "Use generic-lens or generic-optics with 'aliasName' instead." #-}

-- | String that contains the key ARN.
--
-- /Note:/ Consider using 'aliasARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aleAliasARN :: Lens.Lens' AliasListEntry (Lude.Maybe Lude.Text)
aleAliasARN = Lens.lens (aliasARN :: AliasListEntry -> Lude.Maybe Lude.Text) (\s a -> s {aliasARN = a} :: AliasListEntry)
{-# DEPRECATED aleAliasARN "Use generic-lens or generic-optics with 'aliasARN' instead." #-}

instance Lude.FromJSON AliasListEntry where
  parseJSON =
    Lude.withObject
      "AliasListEntry"
      ( \x ->
          AliasListEntry'
            Lude.<$> (x Lude..:? "TargetKeyId")
            Lude.<*> (x Lude..:? "AliasName")
            Lude.<*> (x Lude..:? "AliasArn")
      )
