{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SubscribedWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SubscribedWorkteam where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a work team of a vendor that does the a labelling job.
--
--
--
-- /See:/ 'subscribedWorkteam' smart constructor.
data SubscribedWorkteam = SubscribedWorkteam'
  { _swMarketplaceTitle ::
      !(Maybe Text),
    _swSellerName :: !(Maybe Text),
    _swListingId :: !(Maybe Text),
    _swMarketplaceDescription :: !(Maybe Text),
    _swWorkteamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscribedWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swMarketplaceTitle' - The title of the service provided by the vendor in the Amazon Marketplace.
--
-- * 'swSellerName' - The name of the vendor in the Amazon Marketplace.
--
-- * 'swListingId' - Marketplace product listing ID.
--
-- * 'swMarketplaceDescription' - The description of the vendor from the Amazon Marketplace.
--
-- * 'swWorkteamARN' - The Amazon Resource Name (ARN) of the vendor that you have subscribed.
subscribedWorkteam ::
  -- | 'swWorkteamARN'
  Text ->
  SubscribedWorkteam
subscribedWorkteam pWorkteamARN_ =
  SubscribedWorkteam'
    { _swMarketplaceTitle = Nothing,
      _swSellerName = Nothing,
      _swListingId = Nothing,
      _swMarketplaceDescription = Nothing,
      _swWorkteamARN = pWorkteamARN_
    }

-- | The title of the service provided by the vendor in the Amazon Marketplace.
swMarketplaceTitle :: Lens' SubscribedWorkteam (Maybe Text)
swMarketplaceTitle = lens _swMarketplaceTitle (\s a -> s {_swMarketplaceTitle = a})

-- | The name of the vendor in the Amazon Marketplace.
swSellerName :: Lens' SubscribedWorkteam (Maybe Text)
swSellerName = lens _swSellerName (\s a -> s {_swSellerName = a})

-- | Marketplace product listing ID.
swListingId :: Lens' SubscribedWorkteam (Maybe Text)
swListingId = lens _swListingId (\s a -> s {_swListingId = a})

-- | The description of the vendor from the Amazon Marketplace.
swMarketplaceDescription :: Lens' SubscribedWorkteam (Maybe Text)
swMarketplaceDescription = lens _swMarketplaceDescription (\s a -> s {_swMarketplaceDescription = a})

-- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
swWorkteamARN :: Lens' SubscribedWorkteam Text
swWorkteamARN = lens _swWorkteamARN (\s a -> s {_swWorkteamARN = a})

instance FromJSON SubscribedWorkteam where
  parseJSON =
    withObject
      "SubscribedWorkteam"
      ( \x ->
          SubscribedWorkteam'
            <$> (x .:? "MarketplaceTitle")
            <*> (x .:? "SellerName")
            <*> (x .:? "ListingId")
            <*> (x .:? "MarketplaceDescription")
            <*> (x .: "WorkteamArn")
      )

instance Hashable SubscribedWorkteam

instance NFData SubscribedWorkteam
