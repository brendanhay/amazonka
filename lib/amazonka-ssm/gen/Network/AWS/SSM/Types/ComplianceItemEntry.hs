{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceItemEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItemEntry where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus

-- | Information about a compliance item.
--
--
--
-- /See:/ 'complianceItemEntry' smart constructor.
data ComplianceItemEntry = ComplianceItemEntry'
  { _cieDetails ::
      !(Maybe (Map Text (Text))),
    _cieId :: !(Maybe Text),
    _cieTitle :: !(Maybe Text),
    _cieSeverity :: !ComplianceSeverity,
    _cieStatus :: !ComplianceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceItemEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cieDetails' - A "Key": "Value" tag combination for the compliance item.
--
-- * 'cieId' - The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
--
-- * 'cieTitle' - The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
--
-- * 'cieSeverity' - The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- * 'cieStatus' - The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
complianceItemEntry ::
  -- | 'cieSeverity'
  ComplianceSeverity ->
  -- | 'cieStatus'
  ComplianceStatus ->
  ComplianceItemEntry
complianceItemEntry pSeverity_ pStatus_ =
  ComplianceItemEntry'
    { _cieDetails = Nothing,
      _cieId = Nothing,
      _cieTitle = Nothing,
      _cieSeverity = pSeverity_,
      _cieStatus = pStatus_
    }

-- | A "Key": "Value" tag combination for the compliance item.
cieDetails :: Lens' ComplianceItemEntry (HashMap Text (Text))
cieDetails = lens _cieDetails (\s a -> s {_cieDetails = a}) . _Default . _Map

-- | The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
cieId :: Lens' ComplianceItemEntry (Maybe Text)
cieId = lens _cieId (\s a -> s {_cieId = a})

-- | The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
cieTitle :: Lens' ComplianceItemEntry (Maybe Text)
cieTitle = lens _cieTitle (\s a -> s {_cieTitle = a})

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
cieSeverity :: Lens' ComplianceItemEntry ComplianceSeverity
cieSeverity = lens _cieSeverity (\s a -> s {_cieSeverity = a})

-- | The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
cieStatus :: Lens' ComplianceItemEntry ComplianceStatus
cieStatus = lens _cieStatus (\s a -> s {_cieStatus = a})

instance Hashable ComplianceItemEntry

instance NFData ComplianceItemEntry

instance ToJSON ComplianceItemEntry where
  toJSON ComplianceItemEntry' {..} =
    object
      ( catMaybes
          [ ("Details" .=) <$> _cieDetails,
            ("Id" .=) <$> _cieId,
            ("Title" .=) <$> _cieTitle,
            Just ("Severity" .= _cieSeverity),
            Just ("Status" .= _cieStatus)
          ]
      )
