{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.NonCompliantResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.NonCompliantResource where

import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the resource that was noncompliant with the audit check.
--
--
--
-- /See:/ 'nonCompliantResource' smart constructor.
data NonCompliantResource = NonCompliantResource'
  { _ncrAdditionalInfo ::
      !(Maybe (Map Text (Text))),
    _ncrResourceType :: !(Maybe ResourceType),
    _ncrResourceIdentifier ::
      !(Maybe ResourceIdentifier)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NonCompliantResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncrAdditionalInfo' - Other information about the noncompliant resource.
--
-- * 'ncrResourceType' - The type of the noncompliant resource.
--
-- * 'ncrResourceIdentifier' - Information that identifies the noncompliant resource.
nonCompliantResource ::
  NonCompliantResource
nonCompliantResource =
  NonCompliantResource'
    { _ncrAdditionalInfo = Nothing,
      _ncrResourceType = Nothing,
      _ncrResourceIdentifier = Nothing
    }

-- | Other information about the noncompliant resource.
ncrAdditionalInfo :: Lens' NonCompliantResource (HashMap Text (Text))
ncrAdditionalInfo = lens _ncrAdditionalInfo (\s a -> s {_ncrAdditionalInfo = a}) . _Default . _Map

-- | The type of the noncompliant resource.
ncrResourceType :: Lens' NonCompliantResource (Maybe ResourceType)
ncrResourceType = lens _ncrResourceType (\s a -> s {_ncrResourceType = a})

-- | Information that identifies the noncompliant resource.
ncrResourceIdentifier :: Lens' NonCompliantResource (Maybe ResourceIdentifier)
ncrResourceIdentifier = lens _ncrResourceIdentifier (\s a -> s {_ncrResourceIdentifier = a})

instance FromJSON NonCompliantResource where
  parseJSON =
    withObject
      "NonCompliantResource"
      ( \x ->
          NonCompliantResource'
            <$> (x .:? "additionalInfo" .!= mempty)
            <*> (x .:? "resourceType")
            <*> (x .:? "resourceIdentifier")
      )

instance Hashable NonCompliantResource

instance NFData NonCompliantResource
