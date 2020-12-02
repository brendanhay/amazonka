{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
--
--
--
-- /See:/ 'complianceDetails' smart constructor.
data ComplianceDetails = ComplianceDetails'
  { _cdKeysWithNoncompliantValues ::
      !(Maybe [Text]),
    _cdComplianceStatus :: !(Maybe Bool),
    _cdNoncompliantKeys :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdKeysWithNoncompliantValues' - These are keys defined in the effective policy that are on the resource with either incorrect case treatment or noncompliant values.
--
-- * 'cdComplianceStatus' - Whether a resource is compliant with the effective tag policy.
--
-- * 'cdNoncompliantKeys' - These tag keys on the resource are noncompliant with the effective tag policy.
complianceDetails ::
  ComplianceDetails
complianceDetails =
  ComplianceDetails'
    { _cdKeysWithNoncompliantValues = Nothing,
      _cdComplianceStatus = Nothing,
      _cdNoncompliantKeys = Nothing
    }

-- | These are keys defined in the effective policy that are on the resource with either incorrect case treatment or noncompliant values.
cdKeysWithNoncompliantValues :: Lens' ComplianceDetails [Text]
cdKeysWithNoncompliantValues = lens _cdKeysWithNoncompliantValues (\s a -> s {_cdKeysWithNoncompliantValues = a}) . _Default . _Coerce

-- | Whether a resource is compliant with the effective tag policy.
cdComplianceStatus :: Lens' ComplianceDetails (Maybe Bool)
cdComplianceStatus = lens _cdComplianceStatus (\s a -> s {_cdComplianceStatus = a})

-- | These tag keys on the resource are noncompliant with the effective tag policy.
cdNoncompliantKeys :: Lens' ComplianceDetails [Text]
cdNoncompliantKeys = lens _cdNoncompliantKeys (\s a -> s {_cdNoncompliantKeys = a}) . _Default . _Coerce

instance FromJSON ComplianceDetails where
  parseJSON =
    withObject
      "ComplianceDetails"
      ( \x ->
          ComplianceDetails'
            <$> (x .:? "KeysWithNoncompliantValues" .!= mempty)
            <*> (x .:? "ComplianceStatus")
            <*> (x .:? "NoncompliantKeys" .!= mempty)
      )

instance Hashable ComplianceDetails

instance NFData ComplianceDetails
