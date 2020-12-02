{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AnnotationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AnnotationValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Value of a segment annotation. Has one of three value types: Number, Boolean, or String.
--
--
--
-- /See:/ 'annotationValue' smart constructor.
data AnnotationValue = AnnotationValue'
  { _avNumberValue ::
      !(Maybe Double),
    _avStringValue :: !(Maybe Text),
    _avBooleanValue :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnnotationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avNumberValue' - Value for a Number annotation.
--
-- * 'avStringValue' - Value for a String annotation.
--
-- * 'avBooleanValue' - Value for a Boolean annotation.
annotationValue ::
  AnnotationValue
annotationValue =
  AnnotationValue'
    { _avNumberValue = Nothing,
      _avStringValue = Nothing,
      _avBooleanValue = Nothing
    }

-- | Value for a Number annotation.
avNumberValue :: Lens' AnnotationValue (Maybe Double)
avNumberValue = lens _avNumberValue (\s a -> s {_avNumberValue = a})

-- | Value for a String annotation.
avStringValue :: Lens' AnnotationValue (Maybe Text)
avStringValue = lens _avStringValue (\s a -> s {_avStringValue = a})

-- | Value for a Boolean annotation.
avBooleanValue :: Lens' AnnotationValue (Maybe Bool)
avBooleanValue = lens _avBooleanValue (\s a -> s {_avBooleanValue = a})

instance FromJSON AnnotationValue where
  parseJSON =
    withObject
      "AnnotationValue"
      ( \x ->
          AnnotationValue'
            <$> (x .:? "NumberValue")
            <*> (x .:? "StringValue")
            <*> (x .:? "BooleanValue")
      )

instance Hashable AnnotationValue

instance NFData AnnotationValue
