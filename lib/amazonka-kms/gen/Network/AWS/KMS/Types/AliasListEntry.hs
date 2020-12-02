{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.AliasListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.AliasListEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an alias.
--
--
--
-- /See:/ 'aliasListEntry' smart constructor.
data AliasListEntry = AliasListEntry'
  { _aleTargetKeyId ::
      !(Maybe Text),
    _aleAliasName :: !(Maybe Text),
    _aleAliasARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AliasListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aleTargetKeyId' - String that contains the key identifier referred to by the alias.
--
-- * 'aleAliasName' - String that contains the alias. This value begins with @alias/@ .
--
-- * 'aleAliasARN' - String that contains the key ARN.
aliasListEntry ::
  AliasListEntry
aliasListEntry =
  AliasListEntry'
    { _aleTargetKeyId = Nothing,
      _aleAliasName = Nothing,
      _aleAliasARN = Nothing
    }

-- | String that contains the key identifier referred to by the alias.
aleTargetKeyId :: Lens' AliasListEntry (Maybe Text)
aleTargetKeyId = lens _aleTargetKeyId (\s a -> s {_aleTargetKeyId = a})

-- | String that contains the alias. This value begins with @alias/@ .
aleAliasName :: Lens' AliasListEntry (Maybe Text)
aleAliasName = lens _aleAliasName (\s a -> s {_aleAliasName = a})

-- | String that contains the key ARN.
aleAliasARN :: Lens' AliasListEntry (Maybe Text)
aleAliasARN = lens _aleAliasARN (\s a -> s {_aleAliasARN = a})

instance FromJSON AliasListEntry where
  parseJSON =
    withObject
      "AliasListEntry"
      ( \x ->
          AliasListEntry'
            <$> (x .:? "TargetKeyId")
            <*> (x .:? "AliasName")
            <*> (x .:? "AliasArn")
      )

instance Hashable AliasListEntry

instance NFData AliasListEntry
