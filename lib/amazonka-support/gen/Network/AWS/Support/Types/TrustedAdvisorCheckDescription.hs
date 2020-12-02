{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description and metadata for a Trusted Advisor check.
--
--
--
-- /See:/ 'trustedAdvisorCheckDescription' smart constructor.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription'
  { _tacdId ::
      !Text,
    _tacdName :: !Text,
    _tacdDescription :: !Text,
    _tacdCategory :: !Text,
    _tacdMetadata :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrustedAdvisorCheckDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tacdId' - The unique identifier for the Trusted Advisor check.
--
-- * 'tacdName' - The display name for the Trusted Advisor check.
--
-- * 'tacdDescription' - The description of the Trusted Advisor check, which includes the alert criteria and recommended operations (contains HTML markup).
--
-- * 'tacdCategory' - The category of the Trusted Advisor check.
--
-- * 'tacdMetadata' - The column headings for the data returned by the Trusted Advisor check. The order of the headings corresponds to the order of the data in the __Metadata__ element of the 'TrustedAdvisorResourceDetail' for the check. __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
trustedAdvisorCheckDescription ::
  -- | 'tacdId'
  Text ->
  -- | 'tacdName'
  Text ->
  -- | 'tacdDescription'
  Text ->
  -- | 'tacdCategory'
  Text ->
  TrustedAdvisorCheckDescription
trustedAdvisorCheckDescription pId_ pName_ pDescription_ pCategory_ =
  TrustedAdvisorCheckDescription'
    { _tacdId = pId_,
      _tacdName = pName_,
      _tacdDescription = pDescription_,
      _tacdCategory = pCategory_,
      _tacdMetadata = mempty
    }

-- | The unique identifier for the Trusted Advisor check.
tacdId :: Lens' TrustedAdvisorCheckDescription Text
tacdId = lens _tacdId (\s a -> s {_tacdId = a})

-- | The display name for the Trusted Advisor check.
tacdName :: Lens' TrustedAdvisorCheckDescription Text
tacdName = lens _tacdName (\s a -> s {_tacdName = a})

-- | The description of the Trusted Advisor check, which includes the alert criteria and recommended operations (contains HTML markup).
tacdDescription :: Lens' TrustedAdvisorCheckDescription Text
tacdDescription = lens _tacdDescription (\s a -> s {_tacdDescription = a})

-- | The category of the Trusted Advisor check.
tacdCategory :: Lens' TrustedAdvisorCheckDescription Text
tacdCategory = lens _tacdCategory (\s a -> s {_tacdCategory = a})

-- | The column headings for the data returned by the Trusted Advisor check. The order of the headings corresponds to the order of the data in the __Metadata__ element of the 'TrustedAdvisorResourceDetail' for the check. __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
tacdMetadata :: Lens' TrustedAdvisorCheckDescription [Text]
tacdMetadata = lens _tacdMetadata (\s a -> s {_tacdMetadata = a}) . _Coerce

instance FromJSON TrustedAdvisorCheckDescription where
  parseJSON =
    withObject
      "TrustedAdvisorCheckDescription"
      ( \x ->
          TrustedAdvisorCheckDescription'
            <$> (x .: "id")
            <*> (x .: "name")
            <*> (x .: "description")
            <*> (x .: "category")
            <*> (x .:? "metadata" .!= mempty)
      )

instance Hashable TrustedAdvisorCheckDescription

instance NFData TrustedAdvisorCheckDescription
