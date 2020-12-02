{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyData where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.TerminologyDataFormat

-- | The data associated with the custom terminology.
--
--
--
-- /See:/ 'terminologyData' smart constructor.
data TerminologyData = TerminologyData'
  { _tdFile ::
      !(Sensitive Base64),
    _tdFormat :: !TerminologyDataFormat
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'TerminologyData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdFile' - The file containing the custom terminology data. Your version of the AWS SDK performs a Base64-encoding on this field before sending a request to the AWS service. Users of the SDK should not perform Base64-encoding themselves.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'tdFormat' - The data format of the custom terminology. Either CSV or TMX.
terminologyData ::
  -- | 'tdFile'
  ByteString ->
  -- | 'tdFormat'
  TerminologyDataFormat ->
  TerminologyData
terminologyData pFile_ pFormat_ =
  TerminologyData'
    { _tdFile = _Sensitive . _Base64 # pFile_,
      _tdFormat = pFormat_
    }

-- | The file containing the custom terminology data. Your version of the AWS SDK performs a Base64-encoding on this field before sending a request to the AWS service. Users of the SDK should not perform Base64-encoding themselves.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
tdFile :: Lens' TerminologyData ByteString
tdFile = lens _tdFile (\s a -> s {_tdFile = a}) . _Sensitive . _Base64

-- | The data format of the custom terminology. Either CSV or TMX.
tdFormat :: Lens' TerminologyData TerminologyDataFormat
tdFormat = lens _tdFormat (\s a -> s {_tdFormat = a})

instance Hashable TerminologyData

instance NFData TerminologyData

instance ToJSON TerminologyData where
  toJSON TerminologyData' {..} =
    object
      ( catMaybes
          [Just ("File" .= _tdFile), Just ("Format" .= _tdFormat)]
      )
