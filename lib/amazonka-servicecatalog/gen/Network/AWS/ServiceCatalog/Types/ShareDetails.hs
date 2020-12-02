{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ShareError

-- | Information about the portfolio share operation.
--
--
--
-- /See:/ 'shareDetails' smart constructor.
data ShareDetails = ShareDetails'
  { _sdShareErrors ::
      !(Maybe [ShareError]),
    _sdSuccessfulShares :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShareDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdShareErrors' - List of errors.
--
-- * 'sdSuccessfulShares' - List of accounts for whom the operation succeeded.
shareDetails ::
  ShareDetails
shareDetails =
  ShareDetails'
    { _sdShareErrors = Nothing,
      _sdSuccessfulShares = Nothing
    }

-- | List of errors.
sdShareErrors :: Lens' ShareDetails [ShareError]
sdShareErrors = lens _sdShareErrors (\s a -> s {_sdShareErrors = a}) . _Default . _Coerce

-- | List of accounts for whom the operation succeeded.
sdSuccessfulShares :: Lens' ShareDetails [Text]
sdSuccessfulShares = lens _sdSuccessfulShares (\s a -> s {_sdSuccessfulShares = a}) . _Default . _Coerce

instance FromJSON ShareDetails where
  parseJSON =
    withObject
      "ShareDetails"
      ( \x ->
          ShareDetails'
            <$> (x .:? "ShareErrors" .!= mempty)
            <*> (x .:? "SuccessfulShares" .!= mempty)
      )

instance Hashable ShareDetails

instance NFData ShareDetails
