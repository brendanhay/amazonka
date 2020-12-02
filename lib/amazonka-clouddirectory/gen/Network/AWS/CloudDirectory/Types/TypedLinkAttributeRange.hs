{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange where

import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the range of attributes that are used by a specified filter.
--
--
--
-- /See:/ 'typedLinkAttributeRange' smart constructor.
data TypedLinkAttributeRange = TypedLinkAttributeRange'
  { _tlarAttributeName ::
      !(Maybe Text),
    _tlarRange :: !TypedAttributeValueRange
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedLinkAttributeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlarAttributeName' - The unique name of the typed link attribute.
--
-- * 'tlarRange' - The range of attribute values that are being selected.
typedLinkAttributeRange ::
  -- | 'tlarRange'
  TypedAttributeValueRange ->
  TypedLinkAttributeRange
typedLinkAttributeRange pRange_ =
  TypedLinkAttributeRange'
    { _tlarAttributeName = Nothing,
      _tlarRange = pRange_
    }

-- | The unique name of the typed link attribute.
tlarAttributeName :: Lens' TypedLinkAttributeRange (Maybe Text)
tlarAttributeName = lens _tlarAttributeName (\s a -> s {_tlarAttributeName = a})

-- | The range of attribute values that are being selected.
tlarRange :: Lens' TypedLinkAttributeRange TypedAttributeValueRange
tlarRange = lens _tlarRange (\s a -> s {_tlarRange = a})

instance Hashable TypedLinkAttributeRange

instance NFData TypedLinkAttributeRange

instance ToJSON TypedLinkAttributeRange where
  toJSON TypedLinkAttributeRange' {..} =
    object
      ( catMaybes
          [ ("AttributeName" .=) <$> _tlarAttributeName,
            Just ("Range" .= _tlarRange)
          ]
      )
