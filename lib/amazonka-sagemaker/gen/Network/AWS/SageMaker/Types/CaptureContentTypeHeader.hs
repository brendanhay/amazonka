{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureContentTypeHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureContentTypeHeader where

import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'captureContentTypeHeader' smart constructor.
data CaptureContentTypeHeader = CaptureContentTypeHeader'
  { _ccthCSVContentTypes ::
      !(Maybe (List1 Text)),
    _ccthJSONContentTypes ::
      !(Maybe (List1 Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptureContentTypeHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccthCSVContentTypes' -
--
-- * 'ccthJSONContentTypes' -
captureContentTypeHeader ::
  CaptureContentTypeHeader
captureContentTypeHeader =
  CaptureContentTypeHeader'
    { _ccthCSVContentTypes = Nothing,
      _ccthJSONContentTypes = Nothing
    }

-- |
ccthCSVContentTypes :: Lens' CaptureContentTypeHeader (Maybe (NonEmpty Text))
ccthCSVContentTypes = lens _ccthCSVContentTypes (\s a -> s {_ccthCSVContentTypes = a}) . mapping _List1

-- |
ccthJSONContentTypes :: Lens' CaptureContentTypeHeader (Maybe (NonEmpty Text))
ccthJSONContentTypes = lens _ccthJSONContentTypes (\s a -> s {_ccthJSONContentTypes = a}) . mapping _List1

instance FromJSON CaptureContentTypeHeader where
  parseJSON =
    withObject
      "CaptureContentTypeHeader"
      ( \x ->
          CaptureContentTypeHeader'
            <$> (x .:? "CsvContentTypes") <*> (x .:? "JsonContentTypes")
      )

instance Hashable CaptureContentTypeHeader

instance NFData CaptureContentTypeHeader

instance ToJSON CaptureContentTypeHeader where
  toJSON CaptureContentTypeHeader' {..} =
    object
      ( catMaybes
          [ ("CsvContentTypes" .=) <$> _ccthCSVContentTypes,
            ("JsonContentTypes" .=) <$> _ccthJSONContentTypes
          ]
      )
