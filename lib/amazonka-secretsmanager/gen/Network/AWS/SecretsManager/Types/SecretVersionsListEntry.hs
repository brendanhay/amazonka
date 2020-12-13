{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.SecretVersionsListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.SecretVersionsListEntry
  ( SecretVersionsListEntry (..),

    -- * Smart constructor
    mkSecretVersionsListEntry,

    -- * Lenses
    svleVersionId,
    svleVersionStages,
    svleCreatedDate,
    svleLastAccessedDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains information about one version of a secret.
--
-- /See:/ 'mkSecretVersionsListEntry' smart constructor.
data SecretVersionsListEntry = SecretVersionsListEntry'
  { -- | The unique version identifier of this version of the secret.
    versionId :: Lude.Maybe Lude.Text,
    -- | An array of staging labels that are currently associated with this version of the secret.
    versionStages :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The date and time this version of the secret was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
    lastAccessedDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecretVersionsListEntry' with the minimum fields required to make a request.
--
-- * 'versionId' - The unique version identifier of this version of the secret.
-- * 'versionStages' - An array of staging labels that are currently associated with this version of the secret.
-- * 'createdDate' - The date and time this version of the secret was created.
-- * 'lastAccessedDate' - The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
mkSecretVersionsListEntry ::
  SecretVersionsListEntry
mkSecretVersionsListEntry =
  SecretVersionsListEntry'
    { versionId = Lude.Nothing,
      versionStages = Lude.Nothing,
      createdDate = Lude.Nothing,
      lastAccessedDate = Lude.Nothing
    }

-- | The unique version identifier of this version of the secret.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleVersionId :: Lens.Lens' SecretVersionsListEntry (Lude.Maybe Lude.Text)
svleVersionId = Lens.lens (versionId :: SecretVersionsListEntry -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: SecretVersionsListEntry)
{-# DEPRECATED svleVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | An array of staging labels that are currently associated with this version of the secret.
--
-- /Note:/ Consider using 'versionStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleVersionStages :: Lens.Lens' SecretVersionsListEntry (Lude.Maybe (Lude.NonEmpty Lude.Text))
svleVersionStages = Lens.lens (versionStages :: SecretVersionsListEntry -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {versionStages = a} :: SecretVersionsListEntry)
{-# DEPRECATED svleVersionStages "Use generic-lens or generic-optics with 'versionStages' instead." #-}

-- | The date and time this version of the secret was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleCreatedDate :: Lens.Lens' SecretVersionsListEntry (Lude.Maybe Lude.Timestamp)
svleCreatedDate = Lens.lens (createdDate :: SecretVersionsListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: SecretVersionsListEntry)
{-# DEPRECATED svleCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
--
-- /Note:/ Consider using 'lastAccessedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svleLastAccessedDate :: Lens.Lens' SecretVersionsListEntry (Lude.Maybe Lude.Timestamp)
svleLastAccessedDate = Lens.lens (lastAccessedDate :: SecretVersionsListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessedDate = a} :: SecretVersionsListEntry)
{-# DEPRECATED svleLastAccessedDate "Use generic-lens or generic-optics with 'lastAccessedDate' instead." #-}

instance Lude.FromJSON SecretVersionsListEntry where
  parseJSON =
    Lude.withObject
      "SecretVersionsListEntry"
      ( \x ->
          SecretVersionsListEntry'
            Lude.<$> (x Lude..:? "VersionId")
            Lude.<*> (x Lude..:? "VersionStages")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "LastAccessedDate")
      )
