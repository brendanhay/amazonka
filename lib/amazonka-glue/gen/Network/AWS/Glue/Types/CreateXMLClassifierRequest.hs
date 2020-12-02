{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateXMLClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateXMLClassifierRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an XML classifier for @CreateClassifier@ to create.
--
--
--
-- /See:/ 'createXMLClassifierRequest' smart constructor.
data CreateXMLClassifierRequest = CreateXMLClassifierRequest'
  { _cxcrRowTag ::
      !(Maybe Text),
    _cxcrClassification :: !Text,
    _cxcrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateXMLClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cxcrRowTag' - The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- * 'cxcrClassification' - An identifier of the data format that the classifier matches.
--
-- * 'cxcrName' - The name of the classifier.
createXMLClassifierRequest ::
  -- | 'cxcrClassification'
  Text ->
  -- | 'cxcrName'
  Text ->
  CreateXMLClassifierRequest
createXMLClassifierRequest pClassification_ pName_ =
  CreateXMLClassifierRequest'
    { _cxcrRowTag = Nothing,
      _cxcrClassification = pClassification_,
      _cxcrName = pName_
    }

-- | The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
cxcrRowTag :: Lens' CreateXMLClassifierRequest (Maybe Text)
cxcrRowTag = lens _cxcrRowTag (\s a -> s {_cxcrRowTag = a})

-- | An identifier of the data format that the classifier matches.
cxcrClassification :: Lens' CreateXMLClassifierRequest Text
cxcrClassification = lens _cxcrClassification (\s a -> s {_cxcrClassification = a})

-- | The name of the classifier.
cxcrName :: Lens' CreateXMLClassifierRequest Text
cxcrName = lens _cxcrName (\s a -> s {_cxcrName = a})

instance Hashable CreateXMLClassifierRequest

instance NFData CreateXMLClassifierRequest

instance ToJSON CreateXMLClassifierRequest where
  toJSON CreateXMLClassifierRequest' {..} =
    object
      ( catMaybes
          [ ("RowTag" .=) <$> _cxcrRowTag,
            Just ("Classification" .= _cxcrClassification),
            Just ("Name" .= _cxcrName)
          ]
      )
