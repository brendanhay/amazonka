{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.InputTransformer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.InputTransformer where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.
--
--
--
-- /See:/ 'inputTransformer' smart constructor.
data InputTransformer = InputTransformer'
  { _itInputPathsMap ::
      !(Maybe (Map Text (Text))),
    _itInputTemplate :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputTransformer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itInputPathsMap' - Map of JSON paths to be extracted from the event. You can then insert these in the template in @InputTemplate@ to produce the output you want to be sent to the target. @InputPathsMap@ is an array key-value pairs, where each value is a valid JSON path. You can have as many as 10 key-value pairs. You must use JSON dot notation, not bracket notation. The keys cannot start with "AWS."
--
-- * 'itInputTemplate' - Input template where you specify placeholders that will be filled with the values of the keys from @InputPathsMap@ to customize the data sent to the target. Enclose each @InputPathsMaps@ value in brackets: </value/ > The InputTemplate must be valid JSON. If @InputTemplate@ is a JSON object (surrounded by curly braces), the following restrictions apply:     * The placeholder cannot be used as an object key.     * Object values cannot include quote marks. The following example shows the syntax for using @InputPathsMap@ and @InputTemplate@ . @"InputTransformer":@  @{@  @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@  @"InputTemplate": "<instance> is in state <status>"@  @}@  To have the @InputTemplate@ include quote marks within a JSON string, escape each quote marks with a slash, as in the following example: @"InputTransformer":@  @{@  @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@  @"InputTemplate": "<instance> is in state \"<status>\""@  @}@
inputTransformer ::
  -- | 'itInputTemplate'
  Text ->
  InputTransformer
inputTransformer pInputTemplate_ =
  InputTransformer'
    { _itInputPathsMap = Nothing,
      _itInputTemplate = pInputTemplate_
    }

-- | Map of JSON paths to be extracted from the event. You can then insert these in the template in @InputTemplate@ to produce the output you want to be sent to the target. @InputPathsMap@ is an array key-value pairs, where each value is a valid JSON path. You can have as many as 10 key-value pairs. You must use JSON dot notation, not bracket notation. The keys cannot start with "AWS."
itInputPathsMap :: Lens' InputTransformer (HashMap Text (Text))
itInputPathsMap = lens _itInputPathsMap (\s a -> s {_itInputPathsMap = a}) . _Default . _Map

-- | Input template where you specify placeholders that will be filled with the values of the keys from @InputPathsMap@ to customize the data sent to the target. Enclose each @InputPathsMaps@ value in brackets: </value/ > The InputTemplate must be valid JSON. If @InputTemplate@ is a JSON object (surrounded by curly braces), the following restrictions apply:     * The placeholder cannot be used as an object key.     * Object values cannot include quote marks. The following example shows the syntax for using @InputPathsMap@ and @InputTemplate@ . @"InputTransformer":@  @{@  @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@  @"InputTemplate": "<instance> is in state <status>"@  @}@  To have the @InputTemplate@ include quote marks within a JSON string, escape each quote marks with a slash, as in the following example: @"InputTransformer":@  @{@  @"InputPathsMap": {"instance": "$.detail.instance","status": "$.detail.status"},@  @"InputTemplate": "<instance> is in state \"<status>\""@  @}@
itInputTemplate :: Lens' InputTransformer Text
itInputTemplate = lens _itInputTemplate (\s a -> s {_itInputTemplate = a})

instance FromJSON InputTransformer where
  parseJSON =
    withObject
      "InputTransformer"
      ( \x ->
          InputTransformer'
            <$> (x .:? "InputPathsMap" .!= mempty) <*> (x .: "InputTemplate")
      )

instance Hashable InputTransformer

instance NFData InputTransformer

instance ToJSON InputTransformer where
  toJSON InputTransformer' {..} =
    object
      ( catMaybes
          [ ("InputPathsMap" .=) <$> _itInputPathsMap,
            Just ("InputTemplate" .= _itInputTemplate)
          ]
      )
