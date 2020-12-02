{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of information about a fleet provision template version.
--
--
--
-- /See:/ 'provisioningTemplateVersionSummary' smart constructor.
data ProvisioningTemplateVersionSummary = ProvisioningTemplateVersionSummary'
  { _ptvsVersionId ::
      !(Maybe Int),
    _ptvsCreationDate ::
      !(Maybe POSIX),
    _ptvsIsDefaultVersion ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningTemplateVersionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptvsVersionId' - The ID of the fleet privisioning template version.
--
-- * 'ptvsCreationDate' - The date when the fleet provisioning template version was created
--
-- * 'ptvsIsDefaultVersion' - True if the fleet provisioning template version is the default version, otherwise false.
provisioningTemplateVersionSummary ::
  ProvisioningTemplateVersionSummary
provisioningTemplateVersionSummary =
  ProvisioningTemplateVersionSummary'
    { _ptvsVersionId = Nothing,
      _ptvsCreationDate = Nothing,
      _ptvsIsDefaultVersion = Nothing
    }

-- | The ID of the fleet privisioning template version.
ptvsVersionId :: Lens' ProvisioningTemplateVersionSummary (Maybe Int)
ptvsVersionId = lens _ptvsVersionId (\s a -> s {_ptvsVersionId = a})

-- | The date when the fleet provisioning template version was created
ptvsCreationDate :: Lens' ProvisioningTemplateVersionSummary (Maybe UTCTime)
ptvsCreationDate = lens _ptvsCreationDate (\s a -> s {_ptvsCreationDate = a}) . mapping _Time

-- | True if the fleet provisioning template version is the default version, otherwise false.
ptvsIsDefaultVersion :: Lens' ProvisioningTemplateVersionSummary (Maybe Bool)
ptvsIsDefaultVersion = lens _ptvsIsDefaultVersion (\s a -> s {_ptvsIsDefaultVersion = a})

instance FromJSON ProvisioningTemplateVersionSummary where
  parseJSON =
    withObject
      "ProvisioningTemplateVersionSummary"
      ( \x ->
          ProvisioningTemplateVersionSummary'
            <$> (x .:? "versionId")
            <*> (x .:? "creationDate")
            <*> (x .:? "isDefaultVersion")
      )

instance Hashable ProvisioningTemplateVersionSummary

instance NFData ProvisioningTemplateVersionSummary
