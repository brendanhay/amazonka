{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AttributePayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AttributePayload where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The attribute payload.
--
--
--
-- /See:/ 'attributePayload' smart constructor.
data AttributePayload = AttributePayload'
  { _apAttributes ::
      !(Maybe (Map Text (Text))),
    _apMerge :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributePayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apAttributes' - A JSON string containing up to three key-value pair in JSON format. For example: @{\"attributes\":{\"string1\":\"string2\"}}@
--
-- * 'apMerge' - Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them. To remove an attribute, call @UpdateThing@ with an empty attribute value.
attributePayload ::
  AttributePayload
attributePayload =
  AttributePayload' {_apAttributes = Nothing, _apMerge = Nothing}

-- | A JSON string containing up to three key-value pair in JSON format. For example: @{\"attributes\":{\"string1\":\"string2\"}}@
apAttributes :: Lens' AttributePayload (HashMap Text (Text))
apAttributes = lens _apAttributes (\s a -> s {_apAttributes = a}) . _Default . _Map

-- | Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them. To remove an attribute, call @UpdateThing@ with an empty attribute value.
apMerge :: Lens' AttributePayload (Maybe Bool)
apMerge = lens _apMerge (\s a -> s {_apMerge = a})

instance FromJSON AttributePayload where
  parseJSON =
    withObject
      "AttributePayload"
      ( \x ->
          AttributePayload'
            <$> (x .:? "attributes" .!= mempty) <*> (x .:? "merge")
      )

instance Hashable AttributePayload

instance NFData AttributePayload

instance ToJSON AttributePayload where
  toJSON AttributePayload' {..} =
    object
      ( catMaybes
          [("attributes" .=) <$> _apAttributes, ("merge" .=) <$> _apMerge]
      )
