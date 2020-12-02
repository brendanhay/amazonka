{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisScheme where

import Network.AWS.CloudSearch.Types.AnalysisOptions
import Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for an analysis scheme. Each analysis scheme has a unique name and specifies the language of the text to be processed. The following options can be configured for an analysis scheme: @Synonyms@ , @Stopwords@ , @StemmingDictionary@ , @JapaneseTokenizationDictionary@ and @AlgorithmicStemming@ .
--
--
--
-- /See:/ 'analysisScheme' smart constructor.
data AnalysisScheme = AnalysisScheme'
  { _asAnalysisOptions ::
      !(Maybe AnalysisOptions),
    _asAnalysisSchemeName :: !Text,
    _asAnalysisSchemeLanguage :: !AnalysisSchemeLanguage
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalysisScheme' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAnalysisOptions' - Undocumented member.
--
-- * 'asAnalysisSchemeName' - Undocumented member.
--
-- * 'asAnalysisSchemeLanguage' - Undocumented member.
analysisScheme ::
  -- | 'asAnalysisSchemeName'
  Text ->
  -- | 'asAnalysisSchemeLanguage'
  AnalysisSchemeLanguage ->
  AnalysisScheme
analysisScheme pAnalysisSchemeName_ pAnalysisSchemeLanguage_ =
  AnalysisScheme'
    { _asAnalysisOptions = Nothing,
      _asAnalysisSchemeName = pAnalysisSchemeName_,
      _asAnalysisSchemeLanguage = pAnalysisSchemeLanguage_
    }

-- | Undocumented member.
asAnalysisOptions :: Lens' AnalysisScheme (Maybe AnalysisOptions)
asAnalysisOptions = lens _asAnalysisOptions (\s a -> s {_asAnalysisOptions = a})

-- | Undocumented member.
asAnalysisSchemeName :: Lens' AnalysisScheme Text
asAnalysisSchemeName = lens _asAnalysisSchemeName (\s a -> s {_asAnalysisSchemeName = a})

-- | Undocumented member.
asAnalysisSchemeLanguage :: Lens' AnalysisScheme AnalysisSchemeLanguage
asAnalysisSchemeLanguage = lens _asAnalysisSchemeLanguage (\s a -> s {_asAnalysisSchemeLanguage = a})

instance FromXML AnalysisScheme where
  parseXML x =
    AnalysisScheme'
      <$> (x .@? "AnalysisOptions")
      <*> (x .@ "AnalysisSchemeName")
      <*> (x .@ "AnalysisSchemeLanguage")

instance Hashable AnalysisScheme

instance NFData AnalysisScheme

instance ToQuery AnalysisScheme where
  toQuery AnalysisScheme' {..} =
    mconcat
      [ "AnalysisOptions" =: _asAnalysisOptions,
        "AnalysisSchemeName" =: _asAnalysisSchemeName,
        "AnalysisSchemeLanguage" =: _asAnalysisSchemeLanguage
      ]
