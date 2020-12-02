{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RelatedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RelatedResource where

import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a related resource.
--
--
--
-- /See:/ 'relatedResource' smart constructor.
data RelatedResource = RelatedResource'
  { _rrAdditionalInfo ::
      !(Maybe (Map Text (Text))),
    _rrResourceType :: !(Maybe ResourceType),
    _rrResourceIdentifier :: !(Maybe ResourceIdentifier)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelatedResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrAdditionalInfo' - Other information about the resource.
--
-- * 'rrResourceType' - The type of resource.
--
-- * 'rrResourceIdentifier' - Information that identifies the resource.
relatedResource ::
  RelatedResource
relatedResource =
  RelatedResource'
    { _rrAdditionalInfo = Nothing,
      _rrResourceType = Nothing,
      _rrResourceIdentifier = Nothing
    }

-- | Other information about the resource.
rrAdditionalInfo :: Lens' RelatedResource (HashMap Text (Text))
rrAdditionalInfo = lens _rrAdditionalInfo (\s a -> s {_rrAdditionalInfo = a}) . _Default . _Map

-- | The type of resource.
rrResourceType :: Lens' RelatedResource (Maybe ResourceType)
rrResourceType = lens _rrResourceType (\s a -> s {_rrResourceType = a})

-- | Information that identifies the resource.
rrResourceIdentifier :: Lens' RelatedResource (Maybe ResourceIdentifier)
rrResourceIdentifier = lens _rrResourceIdentifier (\s a -> s {_rrResourceIdentifier = a})

instance FromJSON RelatedResource where
  parseJSON =
    withObject
      "RelatedResource"
      ( \x ->
          RelatedResource'
            <$> (x .:? "additionalInfo" .!= mempty)
            <*> (x .:? "resourceType")
            <*> (x .:? "resourceIdentifier")
      )

instance Hashable RelatedResource

instance NFData RelatedResource
