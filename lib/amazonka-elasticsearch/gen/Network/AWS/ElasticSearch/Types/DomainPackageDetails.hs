{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainPackageDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainPackageDetails where

import Network.AWS.ElasticSearch.Types.DomainPackageStatus
import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.PackageType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information on a package that is associated with a domain.
--
--
--
-- /See:/ 'domainPackageDetails' smart constructor.
data DomainPackageDetails = DomainPackageDetails'
  { _dpdLastUpdated ::
      !(Maybe POSIX),
    _dpdPackageId :: !(Maybe Text),
    _dpdPackageType :: !(Maybe PackageType),
    _dpdPackageName :: !(Maybe Text),
    _dpdPackageVersion :: !(Maybe Text),
    _dpdDomainPackageStatus ::
      !(Maybe DomainPackageStatus),
    _dpdDomainName :: !(Maybe Text),
    _dpdErrorDetails :: !(Maybe ErrorDetails),
    _dpdReferencePath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainPackageDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpdLastUpdated' - Timestamp of the most-recent update to the association status.
--
-- * 'dpdPackageId' - Internal ID of the package.
--
-- * 'dpdPackageType' - Currently supports only TXT-DICTIONARY.
--
-- * 'dpdPackageName' - User specified name of the package.
--
-- * 'dpdPackageVersion' - Undocumented member.
--
-- * 'dpdDomainPackageStatus' - State of the association. Values are ASSOCIATING/ASSOCIATION_FAILED/ACTIVE/DISSOCIATING/DISSOCIATION_FAILED.
--
-- * 'dpdDomainName' - Name of the domain you've associated a package with.
--
-- * 'dpdErrorDetails' - Additional information if the package is in an error state. Null otherwise.
--
-- * 'dpdReferencePath' - The relative path on Amazon ES nodes, which can be used as synonym_path when the package is synonym file.
domainPackageDetails ::
  DomainPackageDetails
domainPackageDetails =
  DomainPackageDetails'
    { _dpdLastUpdated = Nothing,
      _dpdPackageId = Nothing,
      _dpdPackageType = Nothing,
      _dpdPackageName = Nothing,
      _dpdPackageVersion = Nothing,
      _dpdDomainPackageStatus = Nothing,
      _dpdDomainName = Nothing,
      _dpdErrorDetails = Nothing,
      _dpdReferencePath = Nothing
    }

-- | Timestamp of the most-recent update to the association status.
dpdLastUpdated :: Lens' DomainPackageDetails (Maybe UTCTime)
dpdLastUpdated = lens _dpdLastUpdated (\s a -> s {_dpdLastUpdated = a}) . mapping _Time

-- | Internal ID of the package.
dpdPackageId :: Lens' DomainPackageDetails (Maybe Text)
dpdPackageId = lens _dpdPackageId (\s a -> s {_dpdPackageId = a})

-- | Currently supports only TXT-DICTIONARY.
dpdPackageType :: Lens' DomainPackageDetails (Maybe PackageType)
dpdPackageType = lens _dpdPackageType (\s a -> s {_dpdPackageType = a})

-- | User specified name of the package.
dpdPackageName :: Lens' DomainPackageDetails (Maybe Text)
dpdPackageName = lens _dpdPackageName (\s a -> s {_dpdPackageName = a})

-- | Undocumented member.
dpdPackageVersion :: Lens' DomainPackageDetails (Maybe Text)
dpdPackageVersion = lens _dpdPackageVersion (\s a -> s {_dpdPackageVersion = a})

-- | State of the association. Values are ASSOCIATING/ASSOCIATION_FAILED/ACTIVE/DISSOCIATING/DISSOCIATION_FAILED.
dpdDomainPackageStatus :: Lens' DomainPackageDetails (Maybe DomainPackageStatus)
dpdDomainPackageStatus = lens _dpdDomainPackageStatus (\s a -> s {_dpdDomainPackageStatus = a})

-- | Name of the domain you've associated a package with.
dpdDomainName :: Lens' DomainPackageDetails (Maybe Text)
dpdDomainName = lens _dpdDomainName (\s a -> s {_dpdDomainName = a})

-- | Additional information if the package is in an error state. Null otherwise.
dpdErrorDetails :: Lens' DomainPackageDetails (Maybe ErrorDetails)
dpdErrorDetails = lens _dpdErrorDetails (\s a -> s {_dpdErrorDetails = a})

-- | The relative path on Amazon ES nodes, which can be used as synonym_path when the package is synonym file.
dpdReferencePath :: Lens' DomainPackageDetails (Maybe Text)
dpdReferencePath = lens _dpdReferencePath (\s a -> s {_dpdReferencePath = a})

instance FromJSON DomainPackageDetails where
  parseJSON =
    withObject
      "DomainPackageDetails"
      ( \x ->
          DomainPackageDetails'
            <$> (x .:? "LastUpdated")
            <*> (x .:? "PackageID")
            <*> (x .:? "PackageType")
            <*> (x .:? "PackageName")
            <*> (x .:? "PackageVersion")
            <*> (x .:? "DomainPackageStatus")
            <*> (x .:? "DomainName")
            <*> (x .:? "ErrorDetails")
            <*> (x .:? "ReferencePath")
      )

instance Hashable DomainPackageDetails

instance NFData DomainPackageDetails
