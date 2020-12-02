{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Celebrity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Celebrity where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.ComparedFace

-- | Provides information about a celebrity recognized by the 'RecognizeCelebrities' operation.
--
--
--
-- /See:/ 'celebrity' smart constructor.
data Celebrity = Celebrity'
  { _cMatchConfidence :: !(Maybe Double),
    _cURLs :: !(Maybe [Text]),
    _cName :: !(Maybe Text),
    _cId :: !(Maybe Text),
    _cFace :: !(Maybe ComparedFace)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Celebrity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cMatchConfidence' - The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
--
-- * 'cURLs' - An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
--
-- * 'cName' - The name of the celebrity.
--
-- * 'cId' - A unique identifier for the celebrity.
--
-- * 'cFace' - Provides information about the celebrity's face, such as its location on the image.
celebrity ::
  Celebrity
celebrity =
  Celebrity'
    { _cMatchConfidence = Nothing,
      _cURLs = Nothing,
      _cName = Nothing,
      _cId = Nothing,
      _cFace = Nothing
    }

-- | The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
cMatchConfidence :: Lens' Celebrity (Maybe Double)
cMatchConfidence = lens _cMatchConfidence (\s a -> s {_cMatchConfidence = a})

-- | An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
cURLs :: Lens' Celebrity [Text]
cURLs = lens _cURLs (\s a -> s {_cURLs = a}) . _Default . _Coerce

-- | The name of the celebrity.
cName :: Lens' Celebrity (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

-- | A unique identifier for the celebrity.
cId :: Lens' Celebrity (Maybe Text)
cId = lens _cId (\s a -> s {_cId = a})

-- | Provides information about the celebrity's face, such as its location on the image.
cFace :: Lens' Celebrity (Maybe ComparedFace)
cFace = lens _cFace (\s a -> s {_cFace = a})

instance FromJSON Celebrity where
  parseJSON =
    withObject
      "Celebrity"
      ( \x ->
          Celebrity'
            <$> (x .:? "MatchConfidence")
            <*> (x .:? "Urls" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Face")
      )

instance Hashable Celebrity

instance NFData Celebrity
