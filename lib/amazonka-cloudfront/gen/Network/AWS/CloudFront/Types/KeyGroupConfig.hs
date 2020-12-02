{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key group configuration.
--
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
--
-- /See:/ 'keyGroupConfig' smart constructor.
data KeyGroupConfig = KeyGroupConfig'
  { _kgcComment :: !(Maybe Text),
    _kgcName :: !Text,
    _kgcItems :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyGroupConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kgcComment' - A comment to describe the key group.
--
-- * 'kgcName' - A name to identify the key group.
--
-- * 'kgcItems' - A list of the identifiers of the public keys in the key group.
keyGroupConfig ::
  -- | 'kgcName'
  Text ->
  KeyGroupConfig
keyGroupConfig pName_ =
  KeyGroupConfig'
    { _kgcComment = Nothing,
      _kgcName = pName_,
      _kgcItems = mempty
    }

-- | A comment to describe the key group.
kgcComment :: Lens' KeyGroupConfig (Maybe Text)
kgcComment = lens _kgcComment (\s a -> s {_kgcComment = a})

-- | A name to identify the key group.
kgcName :: Lens' KeyGroupConfig Text
kgcName = lens _kgcName (\s a -> s {_kgcName = a})

-- | A list of the identifiers of the public keys in the key group.
kgcItems :: Lens' KeyGroupConfig [Text]
kgcItems = lens _kgcItems (\s a -> s {_kgcItems = a}) . _Coerce

instance FromXML KeyGroupConfig where
  parseXML x =
    KeyGroupConfig'
      <$> (x .@? "Comment")
      <*> (x .@ "Name")
      <*> (x .@? "Items" .!@ mempty >>= parseXMLList "PublicKey")

instance Hashable KeyGroupConfig

instance NFData KeyGroupConfig

instance ToXML KeyGroupConfig where
  toXML KeyGroupConfig' {..} =
    mconcat
      [ "Comment" @= _kgcComment,
        "Name" @= _kgcName,
        "Items" @= toXMLList "PublicKey" _kgcItems
      ]
