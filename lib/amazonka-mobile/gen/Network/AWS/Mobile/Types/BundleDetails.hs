{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.BundleDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.BundleDetails where

import Network.AWS.Lens
import Network.AWS.Mobile.Types.Platform
import Network.AWS.Prelude

-- | The details of the bundle.
--
--
--
-- /See:/ 'bundleDetails' smart constructor.
data BundleDetails = BundleDetails'
  { _bdAvailablePlatforms ::
      !(Maybe [Platform]),
    _bdBundleId :: !(Maybe Text),
    _bdVersion :: !(Maybe Text),
    _bdIconURL :: !(Maybe Text),
    _bdTitle :: !(Maybe Text),
    _bdDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BundleDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdAvailablePlatforms' - Undocumented member.
--
-- * 'bdBundleId' - Undocumented member.
--
-- * 'bdVersion' - Undocumented member.
--
-- * 'bdIconURL' - Undocumented member.
--
-- * 'bdTitle' - Undocumented member.
--
-- * 'bdDescription' - Undocumented member.
bundleDetails ::
  BundleDetails
bundleDetails =
  BundleDetails'
    { _bdAvailablePlatforms = Nothing,
      _bdBundleId = Nothing,
      _bdVersion = Nothing,
      _bdIconURL = Nothing,
      _bdTitle = Nothing,
      _bdDescription = Nothing
    }

-- | Undocumented member.
bdAvailablePlatforms :: Lens' BundleDetails [Platform]
bdAvailablePlatforms = lens _bdAvailablePlatforms (\s a -> s {_bdAvailablePlatforms = a}) . _Default . _Coerce

-- | Undocumented member.
bdBundleId :: Lens' BundleDetails (Maybe Text)
bdBundleId = lens _bdBundleId (\s a -> s {_bdBundleId = a})

-- | Undocumented member.
bdVersion :: Lens' BundleDetails (Maybe Text)
bdVersion = lens _bdVersion (\s a -> s {_bdVersion = a})

-- | Undocumented member.
bdIconURL :: Lens' BundleDetails (Maybe Text)
bdIconURL = lens _bdIconURL (\s a -> s {_bdIconURL = a})

-- | Undocumented member.
bdTitle :: Lens' BundleDetails (Maybe Text)
bdTitle = lens _bdTitle (\s a -> s {_bdTitle = a})

-- | Undocumented member.
bdDescription :: Lens' BundleDetails (Maybe Text)
bdDescription = lens _bdDescription (\s a -> s {_bdDescription = a})

instance FromJSON BundleDetails where
  parseJSON =
    withObject
      "BundleDetails"
      ( \x ->
          BundleDetails'
            <$> (x .:? "availablePlatforms" .!= mempty)
            <*> (x .:? "bundleId")
            <*> (x .:? "version")
            <*> (x .:? "iconUrl")
            <*> (x .:? "title")
            <*> (x .:? "description")
      )

instance Hashable BundleDetails

instance NFData BundleDetails
