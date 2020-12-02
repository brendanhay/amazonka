{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessKeyLastUsed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessKeyLastUsed where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the last time an AWS access key was used since IAM began tracking this information on April 22, 2015.
--
--
-- This data type is used as a response element in the 'GetAccessKeyLastUsed' operation.
--
--
-- /See:/ 'accessKeyLastUsed' smart constructor.
data AccessKeyLastUsed = AccessKeyLastUsed'
  { _akluLastUsedDate ::
      !ISO8601,
    _akluServiceName :: !Text,
    _akluRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessKeyLastUsed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akluLastUsedDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user.
--
-- * 'akluServiceName' - The name of the AWS service with which this access key was most recently used. The value of this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM started tracking this information.     * There is no sign-in data associated with the user.
--
-- * 'akluRegion' - The AWS Region where this access key was most recently used. The value for this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user. For more information about AWS Regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
accessKeyLastUsed ::
  -- | 'akluLastUsedDate'
  UTCTime ->
  -- | 'akluServiceName'
  Text ->
  -- | 'akluRegion'
  Text ->
  AccessKeyLastUsed
accessKeyLastUsed pLastUsedDate_ pServiceName_ pRegion_ =
  AccessKeyLastUsed'
    { _akluLastUsedDate = _Time # pLastUsedDate_,
      _akluServiceName = pServiceName_,
      _akluRegion = pRegion_
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the access key was most recently used. This field is null in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user.
akluLastUsedDate :: Lens' AccessKeyLastUsed UTCTime
akluLastUsedDate = lens _akluLastUsedDate (\s a -> s {_akluLastUsedDate = a}) . _Time

-- | The name of the AWS service with which this access key was most recently used. The value of this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM started tracking this information.     * There is no sign-in data associated with the user.
akluServiceName :: Lens' AccessKeyLastUsed Text
akluServiceName = lens _akluServiceName (\s a -> s {_akluServiceName = a})

-- | The AWS Region where this access key was most recently used. The value for this field is "N/A" in the following situations:     * The user does not have an access key.     * An access key exists but has not been used since IAM began tracking this information.     * There is no sign-in data associated with the user. For more information about AWS Regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> in the Amazon Web Services General Reference.
akluRegion :: Lens' AccessKeyLastUsed Text
akluRegion = lens _akluRegion (\s a -> s {_akluRegion = a})

instance FromXML AccessKeyLastUsed where
  parseXML x =
    AccessKeyLastUsed'
      <$> (x .@ "LastUsedDate") <*> (x .@ "ServiceName") <*> (x .@ "Region")

instance Hashable AccessKeyLastUsed

instance NFData AccessKeyLastUsed
