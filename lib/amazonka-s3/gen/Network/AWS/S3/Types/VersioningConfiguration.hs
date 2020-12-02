{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.VersioningConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.BucketVersioningStatus
import Network.AWS.S3.Types.MFADelete

-- | Describes the versioning state of an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTVersioningStatus.html PUT Bucket versioning> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'versioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { _vcStatus ::
      !(Maybe BucketVersioningStatus),
    _vcMFADelete :: !(Maybe MFADelete)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VersioningConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcStatus' - The versioning state of the bucket.
--
-- * 'vcMFADelete' - Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
versioningConfiguration ::
  VersioningConfiguration
versioningConfiguration =
  VersioningConfiguration'
    { _vcStatus = Nothing,
      _vcMFADelete = Nothing
    }

-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe BucketVersioningStatus)
vcStatus = lens _vcStatus (\s a -> s {_vcStatus = a})

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
vcMFADelete :: Lens' VersioningConfiguration (Maybe MFADelete)
vcMFADelete = lens _vcMFADelete (\s a -> s {_vcMFADelete = a})

instance Hashable VersioningConfiguration

instance NFData VersioningConfiguration

instance ToXML VersioningConfiguration where
  toXML VersioningConfiguration' {..} =
    mconcat ["Status" @= _vcStatus, "MfaDelete" @= _vcMFADelete]
