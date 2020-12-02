{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateXMLClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateXMLClassifierRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an XML classifier to be updated.
--
--
--
-- /See:/ 'updateXMLClassifierRequest' smart constructor.
data UpdateXMLClassifierRequest = UpdateXMLClassifierRequest'
  { _uxcrClassification ::
      !(Maybe Text),
    _uxcrRowTag :: !(Maybe Text),
    _uxcrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateXMLClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uxcrClassification' - An identifier of the data format that the classifier matches.
--
-- * 'uxcrRowTag' - The XML tag designating the element that contains each record in an XML document being parsed. This cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- * 'uxcrName' - The name of the classifier.
updateXMLClassifierRequest ::
  -- | 'uxcrName'
  Text ->
  UpdateXMLClassifierRequest
updateXMLClassifierRequest pName_ =
  UpdateXMLClassifierRequest'
    { _uxcrClassification = Nothing,
      _uxcrRowTag = Nothing,
      _uxcrName = pName_
    }

-- | An identifier of the data format that the classifier matches.
uxcrClassification :: Lens' UpdateXMLClassifierRequest (Maybe Text)
uxcrClassification = lens _uxcrClassification (\s a -> s {_uxcrClassification = a})

-- | The XML tag designating the element that contains each record in an XML document being parsed. This cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
uxcrRowTag :: Lens' UpdateXMLClassifierRequest (Maybe Text)
uxcrRowTag = lens _uxcrRowTag (\s a -> s {_uxcrRowTag = a})

-- | The name of the classifier.
uxcrName :: Lens' UpdateXMLClassifierRequest Text
uxcrName = lens _uxcrName (\s a -> s {_uxcrName = a})

instance Hashable UpdateXMLClassifierRequest

instance NFData UpdateXMLClassifierRequest

instance ToJSON UpdateXMLClassifierRequest where
  toJSON UpdateXMLClassifierRequest' {..} =
    object
      ( catMaybes
          [ ("Classification" .=) <$> _uxcrClassification,
            ("RowTag" .=) <$> _uxcrRowTag,
            Just ("Name" .= _uxcrName)
          ]
      )
