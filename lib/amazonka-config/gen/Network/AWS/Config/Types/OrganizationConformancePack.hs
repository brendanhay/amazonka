{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePack where

import Network.AWS.Config.Types.ConformancePackInputParameter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An organization conformance pack that has information about conformance packs that AWS Config creates in member accounts.
--
--
--
-- /See:/ 'organizationConformancePack' smart constructor.
data OrganizationConformancePack = OrganizationConformancePack'
  { _ocpDeliveryS3Bucket ::
      !(Maybe Text),
    _ocpDeliveryS3KeyPrefix ::
      !(Maybe Text),
    _ocpConformancePackInputParameters ::
      !( Maybe
           [ConformancePackInputParameter]
       ),
    _ocpExcludedAccounts ::
      !(Maybe [Text]),
    _ocpOrganizationConformancePackName ::
      !Text,
    _ocpOrganizationConformancePackARN ::
      !Text,
    _ocpLastUpdateTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationConformancePack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocpDeliveryS3Bucket' - Location of an Amazon S3 bucket where AWS Config can deliver evaluation results and conformance pack template that is used to create a pack.
--
-- * 'ocpDeliveryS3KeyPrefix' - Any folder structure you want to add to an Amazon S3 bucket.
--
-- * 'ocpConformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- * 'ocpExcludedAccounts' - A comma-separated list of accounts excluded from organization conformance pack.
--
-- * 'ocpOrganizationConformancePackName' - The name you assign to an organization conformance pack.
--
-- * 'ocpOrganizationConformancePackARN' - Amazon Resource Name (ARN) of organization conformance pack.
--
-- * 'ocpLastUpdateTime' - Last time when organization conformation pack was updated.
organizationConformancePack ::
  -- | 'ocpOrganizationConformancePackName'
  Text ->
  -- | 'ocpOrganizationConformancePackARN'
  Text ->
  -- | 'ocpLastUpdateTime'
  UTCTime ->
  OrganizationConformancePack
organizationConformancePack
  pOrganizationConformancePackName_
  pOrganizationConformancePackARN_
  pLastUpdateTime_ =
    OrganizationConformancePack'
      { _ocpDeliveryS3Bucket = Nothing,
        _ocpDeliveryS3KeyPrefix = Nothing,
        _ocpConformancePackInputParameters = Nothing,
        _ocpExcludedAccounts = Nothing,
        _ocpOrganizationConformancePackName =
          pOrganizationConformancePackName_,
        _ocpOrganizationConformancePackARN =
          pOrganizationConformancePackARN_,
        _ocpLastUpdateTime = _Time # pLastUpdateTime_
      }

-- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results and conformance pack template that is used to create a pack.
ocpDeliveryS3Bucket :: Lens' OrganizationConformancePack (Maybe Text)
ocpDeliveryS3Bucket = lens _ocpDeliveryS3Bucket (\s a -> s {_ocpDeliveryS3Bucket = a})

-- | Any folder structure you want to add to an Amazon S3 bucket.
ocpDeliveryS3KeyPrefix :: Lens' OrganizationConformancePack (Maybe Text)
ocpDeliveryS3KeyPrefix = lens _ocpDeliveryS3KeyPrefix (\s a -> s {_ocpDeliveryS3KeyPrefix = a})

-- | A list of @ConformancePackInputParameter@ objects.
ocpConformancePackInputParameters :: Lens' OrganizationConformancePack [ConformancePackInputParameter]
ocpConformancePackInputParameters = lens _ocpConformancePackInputParameters (\s a -> s {_ocpConformancePackInputParameters = a}) . _Default . _Coerce

-- | A comma-separated list of accounts excluded from organization conformance pack.
ocpExcludedAccounts :: Lens' OrganizationConformancePack [Text]
ocpExcludedAccounts = lens _ocpExcludedAccounts (\s a -> s {_ocpExcludedAccounts = a}) . _Default . _Coerce

-- | The name you assign to an organization conformance pack.
ocpOrganizationConformancePackName :: Lens' OrganizationConformancePack Text
ocpOrganizationConformancePackName = lens _ocpOrganizationConformancePackName (\s a -> s {_ocpOrganizationConformancePackName = a})

-- | Amazon Resource Name (ARN) of organization conformance pack.
ocpOrganizationConformancePackARN :: Lens' OrganizationConformancePack Text
ocpOrganizationConformancePackARN = lens _ocpOrganizationConformancePackARN (\s a -> s {_ocpOrganizationConformancePackARN = a})

-- | Last time when organization conformation pack was updated.
ocpLastUpdateTime :: Lens' OrganizationConformancePack UTCTime
ocpLastUpdateTime = lens _ocpLastUpdateTime (\s a -> s {_ocpLastUpdateTime = a}) . _Time

instance FromJSON OrganizationConformancePack where
  parseJSON =
    withObject
      "OrganizationConformancePack"
      ( \x ->
          OrganizationConformancePack'
            <$> (x .:? "DeliveryS3Bucket")
            <*> (x .:? "DeliveryS3KeyPrefix")
            <*> (x .:? "ConformancePackInputParameters" .!= mempty)
            <*> (x .:? "ExcludedAccounts" .!= mempty)
            <*> (x .: "OrganizationConformancePackName")
            <*> (x .: "OrganizationConformancePackArn")
            <*> (x .: "LastUpdateTime")
      )

instance Hashable OrganizationConformancePack

instance NFData OrganizationConformancePack
