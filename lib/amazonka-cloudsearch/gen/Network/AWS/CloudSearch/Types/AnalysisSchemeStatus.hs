{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisSchemeStatus where

import Network.AWS.CloudSearch.Types.AnalysisScheme
import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status and configuration of an @AnalysisScheme@ .
--
--
--
-- /See:/ 'analysisSchemeStatus' smart constructor.
data AnalysisSchemeStatus = AnalysisSchemeStatus'
  { _assOptions ::
      !AnalysisScheme,
    _assStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalysisSchemeStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assOptions' - Undocumented member.
--
-- * 'assStatus' - Undocumented member.
analysisSchemeStatus ::
  -- | 'assOptions'
  AnalysisScheme ->
  -- | 'assStatus'
  OptionStatus ->
  AnalysisSchemeStatus
analysisSchemeStatus pOptions_ pStatus_ =
  AnalysisSchemeStatus'
    { _assOptions = pOptions_,
      _assStatus = pStatus_
    }

-- | Undocumented member.
assOptions :: Lens' AnalysisSchemeStatus AnalysisScheme
assOptions = lens _assOptions (\s a -> s {_assOptions = a})

-- | Undocumented member.
assStatus :: Lens' AnalysisSchemeStatus OptionStatus
assStatus = lens _assStatus (\s a -> s {_assStatus = a})

instance FromXML AnalysisSchemeStatus where
  parseXML x =
    AnalysisSchemeStatus' <$> (x .@ "Options") <*> (x .@ "Status")

instance Hashable AnalysisSchemeStatus

instance NFData AnalysisSchemeStatus
