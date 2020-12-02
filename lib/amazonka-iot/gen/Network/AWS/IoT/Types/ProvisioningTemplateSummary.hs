{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of information about a fleet provisioning template.
--
--
--
-- /See:/ 'provisioningTemplateSummary' smart constructor.
data ProvisioningTemplateSummary = ProvisioningTemplateSummary'
  { _ptsLastModifiedDate ::
      !(Maybe POSIX),
    _ptsTemplateName :: !(Maybe Text),
    _ptsEnabled :: !(Maybe Bool),
    _ptsCreationDate :: !(Maybe POSIX),
    _ptsTemplateARN :: !(Maybe Text),
    _ptsDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningTemplateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptsLastModifiedDate' - The date when the fleet provisioning template summary was last modified.
--
-- * 'ptsTemplateName' - The name of the fleet provisioning template.
--
-- * 'ptsEnabled' - True if the fleet provision template is enabled, otherwise false.
--
-- * 'ptsCreationDate' - The date when the fleet provisioning template summary was created.
--
-- * 'ptsTemplateARN' - The ARN of the fleet provisioning template.
--
-- * 'ptsDescription' - The description of the fleet provisioning template.
provisioningTemplateSummary ::
  ProvisioningTemplateSummary
provisioningTemplateSummary =
  ProvisioningTemplateSummary'
    { _ptsLastModifiedDate = Nothing,
      _ptsTemplateName = Nothing,
      _ptsEnabled = Nothing,
      _ptsCreationDate = Nothing,
      _ptsTemplateARN = Nothing,
      _ptsDescription = Nothing
    }

-- | The date when the fleet provisioning template summary was last modified.
ptsLastModifiedDate :: Lens' ProvisioningTemplateSummary (Maybe UTCTime)
ptsLastModifiedDate = lens _ptsLastModifiedDate (\s a -> s {_ptsLastModifiedDate = a}) . mapping _Time

-- | The name of the fleet provisioning template.
ptsTemplateName :: Lens' ProvisioningTemplateSummary (Maybe Text)
ptsTemplateName = lens _ptsTemplateName (\s a -> s {_ptsTemplateName = a})

-- | True if the fleet provision template is enabled, otherwise false.
ptsEnabled :: Lens' ProvisioningTemplateSummary (Maybe Bool)
ptsEnabled = lens _ptsEnabled (\s a -> s {_ptsEnabled = a})

-- | The date when the fleet provisioning template summary was created.
ptsCreationDate :: Lens' ProvisioningTemplateSummary (Maybe UTCTime)
ptsCreationDate = lens _ptsCreationDate (\s a -> s {_ptsCreationDate = a}) . mapping _Time

-- | The ARN of the fleet provisioning template.
ptsTemplateARN :: Lens' ProvisioningTemplateSummary (Maybe Text)
ptsTemplateARN = lens _ptsTemplateARN (\s a -> s {_ptsTemplateARN = a})

-- | The description of the fleet provisioning template.
ptsDescription :: Lens' ProvisioningTemplateSummary (Maybe Text)
ptsDescription = lens _ptsDescription (\s a -> s {_ptsDescription = a})

instance FromJSON ProvisioningTemplateSummary where
  parseJSON =
    withObject
      "ProvisioningTemplateSummary"
      ( \x ->
          ProvisioningTemplateSummary'
            <$> (x .:? "lastModifiedDate")
            <*> (x .:? "templateName")
            <*> (x .:? "enabled")
            <*> (x .:? "creationDate")
            <*> (x .:? "templateArn")
            <*> (x .:? "description")
      )

instance Hashable ProvisioningTemplateSummary

instance NFData ProvisioningTemplateSummary
