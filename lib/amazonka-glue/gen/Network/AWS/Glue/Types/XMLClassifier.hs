{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.XMLClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.XMLClassifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A classifier for @XML@ content.
--
--
--
-- /See:/ 'xmlClassifier' smart constructor.
data XMLClassifier = XMLClassifier'
  { _xcCreationTime ::
      !(Maybe POSIX),
    _xcLastUpdated :: !(Maybe POSIX),
    _xcVersion :: !(Maybe Integer),
    _xcRowTag :: !(Maybe Text),
    _xcName :: !Text,
    _xcClassification :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'XMLClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xcCreationTime' - The time that this classifier was registered.
--
-- * 'xcLastUpdated' - The time that this classifier was last updated.
--
-- * 'xcVersion' - The version of this classifier.
--
-- * 'xcRowTag' - The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- * 'xcName' - The name of the classifier.
--
-- * 'xcClassification' - An identifier of the data format that the classifier matches.
xmlClassifier ::
  -- | 'xcName'
  Text ->
  -- | 'xcClassification'
  Text ->
  XMLClassifier
xmlClassifier pName_ pClassification_ =
  XMLClassifier'
    { _xcCreationTime = Nothing,
      _xcLastUpdated = Nothing,
      _xcVersion = Nothing,
      _xcRowTag = Nothing,
      _xcName = pName_,
      _xcClassification = pClassification_
    }

-- | The time that this classifier was registered.
xcCreationTime :: Lens' XMLClassifier (Maybe UTCTime)
xcCreationTime = lens _xcCreationTime (\s a -> s {_xcCreationTime = a}) . mapping _Time

-- | The time that this classifier was last updated.
xcLastUpdated :: Lens' XMLClassifier (Maybe UTCTime)
xcLastUpdated = lens _xcLastUpdated (\s a -> s {_xcLastUpdated = a}) . mapping _Time

-- | The version of this classifier.
xcVersion :: Lens' XMLClassifier (Maybe Integer)
xcVersion = lens _xcVersion (\s a -> s {_xcVersion = a})

-- | The XML tag designating the element that contains each record in an XML document being parsed. This can't identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
xcRowTag :: Lens' XMLClassifier (Maybe Text)
xcRowTag = lens _xcRowTag (\s a -> s {_xcRowTag = a})

-- | The name of the classifier.
xcName :: Lens' XMLClassifier Text
xcName = lens _xcName (\s a -> s {_xcName = a})

-- | An identifier of the data format that the classifier matches.
xcClassification :: Lens' XMLClassifier Text
xcClassification = lens _xcClassification (\s a -> s {_xcClassification = a})

instance FromJSON XMLClassifier where
  parseJSON =
    withObject
      "XMLClassifier"
      ( \x ->
          XMLClassifier'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LastUpdated")
            <*> (x .:? "Version")
            <*> (x .:? "RowTag")
            <*> (x .: "Name")
            <*> (x .: "Classification")
      )

instance Hashable XMLClassifier

instance NFData XMLClassifier
