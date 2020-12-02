{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeRange where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A range of attributes.
--
--
--
-- /See:/ 'objectAttributeRange' smart constructor.
data ObjectAttributeRange = ObjectAttributeRange'
  { _oarRange ::
      !(Maybe TypedAttributeValueRange),
    _oarAttributeKey :: !(Maybe AttributeKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectAttributeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oarRange' - The range of attribute values being selected.
--
-- * 'oarAttributeKey' - The key of the attribute that the attribute range covers.
objectAttributeRange ::
  ObjectAttributeRange
objectAttributeRange =
  ObjectAttributeRange'
    { _oarRange = Nothing,
      _oarAttributeKey = Nothing
    }

-- | The range of attribute values being selected.
oarRange :: Lens' ObjectAttributeRange (Maybe TypedAttributeValueRange)
oarRange = lens _oarRange (\s a -> s {_oarRange = a})

-- | The key of the attribute that the attribute range covers.
oarAttributeKey :: Lens' ObjectAttributeRange (Maybe AttributeKey)
oarAttributeKey = lens _oarAttributeKey (\s a -> s {_oarAttributeKey = a})

instance Hashable ObjectAttributeRange

instance NFData ObjectAttributeRange

instance ToJSON ObjectAttributeRange where
  toJSON ObjectAttributeRange' {..} =
    object
      ( catMaybes
          [ ("Range" .=) <$> _oarRange,
            ("AttributeKey" .=) <$> _oarAttributeKey
          ]
      )
