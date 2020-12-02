{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Function
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Function where

import Network.AWS.Greengrass.Types.FunctionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a Lambda function.
--
-- /See:/ 'function' smart constructor.
data Function = Function'
  { _fFunctionARN :: !(Maybe Text),
    _fFunctionConfiguration :: !(Maybe FunctionConfiguration),
    _fId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Function' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFunctionARN' - The ARN of the Lambda function.
--
-- * 'fFunctionConfiguration' - The configuration of the Lambda function.
--
-- * 'fId' - A descriptive or arbitrary ID for the function. This value must be unique within the function definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
function ::
  -- | 'fId'
  Text ->
  Function
function pId_ =
  Function'
    { _fFunctionARN = Nothing,
      _fFunctionConfiguration = Nothing,
      _fId = pId_
    }

-- | The ARN of the Lambda function.
fFunctionARN :: Lens' Function (Maybe Text)
fFunctionARN = lens _fFunctionARN (\s a -> s {_fFunctionARN = a})

-- | The configuration of the Lambda function.
fFunctionConfiguration :: Lens' Function (Maybe FunctionConfiguration)
fFunctionConfiguration = lens _fFunctionConfiguration (\s a -> s {_fFunctionConfiguration = a})

-- | A descriptive or arbitrary ID for the function. This value must be unique within the function definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
fId :: Lens' Function Text
fId = lens _fId (\s a -> s {_fId = a})

instance FromJSON Function where
  parseJSON =
    withObject
      "Function"
      ( \x ->
          Function'
            <$> (x .:? "FunctionArn")
            <*> (x .:? "FunctionConfiguration")
            <*> (x .: "Id")
      )

instance Hashable Function

instance NFData Function

instance ToJSON Function where
  toJSON Function' {..} =
    object
      ( catMaybes
          [ ("FunctionArn" .=) <$> _fFunctionARN,
            ("FunctionConfiguration" .=) <$> _fFunctionConfiguration,
            Just ("Id" .= _fId)
          ]
      )
