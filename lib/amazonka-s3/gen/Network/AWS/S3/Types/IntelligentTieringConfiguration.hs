{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringFilter
import Network.AWS.S3.Types.IntelligentTieringStatus
import Network.AWS.S3.Types.Tiering

-- | Specifies the S3 Intelligent-Tiering configuration for an Amazon S3 bucket.
--
--
-- For information about the S3 Intelligent-Tiering storage class, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
--
-- /See:/ 'intelligentTieringConfiguration' smart constructor.
data IntelligentTieringConfiguration = IntelligentTieringConfiguration'
  { _itcFilter ::
      !( Maybe
           IntelligentTieringFilter
       ),
    _itcId :: !Text,
    _itcStatus ::
      !IntelligentTieringStatus,
    _itcTierings :: ![Tiering]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itcFilter' - Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
--
-- * 'itcId' - The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- * 'itcStatus' - Specifies the status of the configuration.
--
-- * 'itcTierings' - Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
intelligentTieringConfiguration ::
  -- | 'itcId'
  Text ->
  -- | 'itcStatus'
  IntelligentTieringStatus ->
  IntelligentTieringConfiguration
intelligentTieringConfiguration pId_ pStatus_ =
  IntelligentTieringConfiguration'
    { _itcFilter = Nothing,
      _itcId = pId_,
      _itcStatus = pStatus_,
      _itcTierings = mempty
    }

-- | Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
itcFilter :: Lens' IntelligentTieringConfiguration (Maybe IntelligentTieringFilter)
itcFilter = lens _itcFilter (\s a -> s {_itcFilter = a})

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
itcId :: Lens' IntelligentTieringConfiguration Text
itcId = lens _itcId (\s a -> s {_itcId = a})

-- | Specifies the status of the configuration.
itcStatus :: Lens' IntelligentTieringConfiguration IntelligentTieringStatus
itcStatus = lens _itcStatus (\s a -> s {_itcStatus = a})

-- | Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
itcTierings :: Lens' IntelligentTieringConfiguration [Tiering]
itcTierings = lens _itcTierings (\s a -> s {_itcTierings = a}) . _Coerce

instance FromXML IntelligentTieringConfiguration where
  parseXML x =
    IntelligentTieringConfiguration'
      <$> (x .@? "Filter")
      <*> (x .@ "Id")
      <*> (x .@ "Status")
      <*> (parseXMLList "Tiering" x)

instance Hashable IntelligentTieringConfiguration

instance NFData IntelligentTieringConfiguration

instance ToXML IntelligentTieringConfiguration where
  toXML IntelligentTieringConfiguration' {..} =
    mconcat
      [ "Filter" @= _itcFilter,
        "Id" @= _itcId,
        "Status" @= _itcStatus,
        toXMLList "Tiering" _itcTierings
      ]
