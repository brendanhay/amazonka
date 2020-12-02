{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Suggester
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.Suggester where

import Network.AWS.CloudSearch.Types.DocumentSuggesterOptions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for a search suggester. Each suggester has a unique name and specifies the text field you want to use for suggestions. The following options can be configured for a suggester: @FuzzyMatching@ , @SortExpression@ .
--
--
--
-- /See:/ 'suggester' smart constructor.
data Suggester = Suggester'
  { _sSuggesterName :: !Text,
    _sDocumentSuggesterOptions :: !DocumentSuggesterOptions
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Suggester' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSuggesterName' - Undocumented member.
--
-- * 'sDocumentSuggesterOptions' - Undocumented member.
suggester ::
  -- | 'sSuggesterName'
  Text ->
  -- | 'sDocumentSuggesterOptions'
  DocumentSuggesterOptions ->
  Suggester
suggester pSuggesterName_ pDocumentSuggesterOptions_ =
  Suggester'
    { _sSuggesterName = pSuggesterName_,
      _sDocumentSuggesterOptions = pDocumentSuggesterOptions_
    }

-- | Undocumented member.
sSuggesterName :: Lens' Suggester Text
sSuggesterName = lens _sSuggesterName (\s a -> s {_sSuggesterName = a})

-- | Undocumented member.
sDocumentSuggesterOptions :: Lens' Suggester DocumentSuggesterOptions
sDocumentSuggesterOptions = lens _sDocumentSuggesterOptions (\s a -> s {_sDocumentSuggesterOptions = a})

instance FromXML Suggester where
  parseXML x =
    Suggester'
      <$> (x .@ "SuggesterName") <*> (x .@ "DocumentSuggesterOptions")

instance Hashable Suggester

instance NFData Suggester

instance ToQuery Suggester where
  toQuery Suggester' {..} =
    mconcat
      [ "SuggesterName" =: _sSuggesterName,
        "DocumentSuggesterOptions" =: _sDocumentSuggesterOptions
      ]
