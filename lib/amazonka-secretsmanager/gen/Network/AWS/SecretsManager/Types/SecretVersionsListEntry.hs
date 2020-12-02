{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.SecretVersionsListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.SecretVersionsListEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains information about one version of a secret.
--
--
--
-- /See:/ 'secretVersionsListEntry' smart constructor.
data SecretVersionsListEntry = SecretVersionsListEntry'
  { _svleVersionId ::
      !(Maybe Text),
    _svleVersionStages :: !(Maybe (List1 Text)),
    _svleCreatedDate :: !(Maybe POSIX),
    _svleLastAccessedDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecretVersionsListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svleVersionId' - The unique version identifier of this version of the secret.
--
-- * 'svleVersionStages' - An array of staging labels that are currently associated with this version of the secret.
--
-- * 'svleCreatedDate' - The date and time this version of the secret was created.
--
-- * 'svleLastAccessedDate' - The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
secretVersionsListEntry ::
  SecretVersionsListEntry
secretVersionsListEntry =
  SecretVersionsListEntry'
    { _svleVersionId = Nothing,
      _svleVersionStages = Nothing,
      _svleCreatedDate = Nothing,
      _svleLastAccessedDate = Nothing
    }

-- | The unique version identifier of this version of the secret.
svleVersionId :: Lens' SecretVersionsListEntry (Maybe Text)
svleVersionId = lens _svleVersionId (\s a -> s {_svleVersionId = a})

-- | An array of staging labels that are currently associated with this version of the secret.
svleVersionStages :: Lens' SecretVersionsListEntry (Maybe (NonEmpty Text))
svleVersionStages = lens _svleVersionStages (\s a -> s {_svleVersionStages = a}) . mapping _List1

-- | The date and time this version of the secret was created.
svleCreatedDate :: Lens' SecretVersionsListEntry (Maybe UTCTime)
svleCreatedDate = lens _svleCreatedDate (\s a -> s {_svleCreatedDate = a}) . mapping _Time

-- | The date that this version of the secret was last accessed. Note that the resolution of this field is at the date level and does not include the time.
svleLastAccessedDate :: Lens' SecretVersionsListEntry (Maybe UTCTime)
svleLastAccessedDate = lens _svleLastAccessedDate (\s a -> s {_svleLastAccessedDate = a}) . mapping _Time

instance FromJSON SecretVersionsListEntry where
  parseJSON =
    withObject
      "SecretVersionsListEntry"
      ( \x ->
          SecretVersionsListEntry'
            <$> (x .:? "VersionId")
            <*> (x .:? "VersionStages")
            <*> (x .:? "CreatedDate")
            <*> (x .:? "LastAccessedDate")
      )

instance Hashable SecretVersionsListEntry

instance NFData SecretVersionsListEntry
