{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeySummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a public key.
--
--
--
-- /See:/ 'publicKeySummary' smart constructor.
data PublicKeySummary = PublicKeySummary'
  { _pksComment ::
      !(Maybe Text),
    _pksId :: !Text,
    _pksName :: !Text,
    _pksCreatedTime :: !ISO8601,
    _pksEncodedKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicKeySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pksComment' - A comment to describe the public key.
--
-- * 'pksId' - The identifier of the public key.
--
-- * 'pksName' - A name to help identify the public key.
--
-- * 'pksCreatedTime' - The date and time when the public key was uploaded.
--
-- * 'pksEncodedKey' - The public key.
publicKeySummary ::
  -- | 'pksId'
  Text ->
  -- | 'pksName'
  Text ->
  -- | 'pksCreatedTime'
  UTCTime ->
  -- | 'pksEncodedKey'
  Text ->
  PublicKeySummary
publicKeySummary pId_ pName_ pCreatedTime_ pEncodedKey_ =
  PublicKeySummary'
    { _pksComment = Nothing,
      _pksId = pId_,
      _pksName = pName_,
      _pksCreatedTime = _Time # pCreatedTime_,
      _pksEncodedKey = pEncodedKey_
    }

-- | A comment to describe the public key.
pksComment :: Lens' PublicKeySummary (Maybe Text)
pksComment = lens _pksComment (\s a -> s {_pksComment = a})

-- | The identifier of the public key.
pksId :: Lens' PublicKeySummary Text
pksId = lens _pksId (\s a -> s {_pksId = a})

-- | A name to help identify the public key.
pksName :: Lens' PublicKeySummary Text
pksName = lens _pksName (\s a -> s {_pksName = a})

-- | The date and time when the public key was uploaded.
pksCreatedTime :: Lens' PublicKeySummary UTCTime
pksCreatedTime = lens _pksCreatedTime (\s a -> s {_pksCreatedTime = a}) . _Time

-- | The public key.
pksEncodedKey :: Lens' PublicKeySummary Text
pksEncodedKey = lens _pksEncodedKey (\s a -> s {_pksEncodedKey = a})

instance FromXML PublicKeySummary where
  parseXML x =
    PublicKeySummary'
      <$> (x .@? "Comment")
      <*> (x .@ "Id")
      <*> (x .@ "Name")
      <*> (x .@ "CreatedTime")
      <*> (x .@ "EncodedKey")

instance Hashable PublicKeySummary

instance NFData PublicKeySummary
