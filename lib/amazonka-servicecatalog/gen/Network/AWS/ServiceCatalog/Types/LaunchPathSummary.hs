{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.LaunchPathSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.LaunchPathSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ConstraintSummary
import Network.AWS.ServiceCatalog.Types.Tag

-- | Summary information about a product path for a user.
--
--
--
-- /See:/ 'launchPathSummary' smart constructor.
data LaunchPathSummary = LaunchPathSummary'
  { _lpsConstraintSummaries ::
      !(Maybe [ConstraintSummary]),
    _lpsName :: !(Maybe Text),
    _lpsId :: !(Maybe Text),
    _lpsTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchPathSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpsConstraintSummaries' - The constraints on the portfolio-product relationship.
--
-- * 'lpsName' - The name of the portfolio to which the user was assigned.
--
-- * 'lpsId' - The identifier of the product path.
--
-- * 'lpsTags' - The tags associated with this product path.
launchPathSummary ::
  LaunchPathSummary
launchPathSummary =
  LaunchPathSummary'
    { _lpsConstraintSummaries = Nothing,
      _lpsName = Nothing,
      _lpsId = Nothing,
      _lpsTags = Nothing
    }

-- | The constraints on the portfolio-product relationship.
lpsConstraintSummaries :: Lens' LaunchPathSummary [ConstraintSummary]
lpsConstraintSummaries = lens _lpsConstraintSummaries (\s a -> s {_lpsConstraintSummaries = a}) . _Default . _Coerce

-- | The name of the portfolio to which the user was assigned.
lpsName :: Lens' LaunchPathSummary (Maybe Text)
lpsName = lens _lpsName (\s a -> s {_lpsName = a})

-- | The identifier of the product path.
lpsId :: Lens' LaunchPathSummary (Maybe Text)
lpsId = lens _lpsId (\s a -> s {_lpsId = a})

-- | The tags associated with this product path.
lpsTags :: Lens' LaunchPathSummary [Tag]
lpsTags = lens _lpsTags (\s a -> s {_lpsTags = a}) . _Default . _Coerce

instance FromJSON LaunchPathSummary where
  parseJSON =
    withObject
      "LaunchPathSummary"
      ( \x ->
          LaunchPathSummary'
            <$> (x .:? "ConstraintSummaries" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable LaunchPathSummary

instance NFData LaunchPathSummary
