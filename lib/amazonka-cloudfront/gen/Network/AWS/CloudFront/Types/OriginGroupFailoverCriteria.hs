{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria where

import Network.AWS.CloudFront.Types.StatusCodes
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type that includes information about the failover criteria for an origin group, including the status codes for which CloudFront will failover from the primary origin to the second origin.
--
--
--
-- /See:/ 'originGroupFailoverCriteria' smart constructor.
newtype OriginGroupFailoverCriteria = OriginGroupFailoverCriteria'
  { _ogfcStatusCodes ::
      StatusCodes
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginGroupFailoverCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogfcStatusCodes' - The status codes that, when returned from the primary origin, will trigger CloudFront to failover to the second origin.
originGroupFailoverCriteria ::
  -- | 'ogfcStatusCodes'
  StatusCodes ->
  OriginGroupFailoverCriteria
originGroupFailoverCriteria pStatusCodes_ =
  OriginGroupFailoverCriteria' {_ogfcStatusCodes = pStatusCodes_}

-- | The status codes that, when returned from the primary origin, will trigger CloudFront to failover to the second origin.
ogfcStatusCodes :: Lens' OriginGroupFailoverCriteria StatusCodes
ogfcStatusCodes = lens _ogfcStatusCodes (\s a -> s {_ogfcStatusCodes = a})

instance FromXML OriginGroupFailoverCriteria where
  parseXML x = OriginGroupFailoverCriteria' <$> (x .@ "StatusCodes")

instance Hashable OriginGroupFailoverCriteria

instance NFData OriginGroupFailoverCriteria

instance ToXML OriginGroupFailoverCriteria where
  toXML OriginGroupFailoverCriteria' {..} =
    mconcat ["StatusCodes" @= _ogfcStatusCodes]
