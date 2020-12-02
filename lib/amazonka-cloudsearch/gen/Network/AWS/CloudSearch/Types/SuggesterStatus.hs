{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.SuggesterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.SuggesterStatus where

import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.CloudSearch.Types.Suggester
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The value of a @Suggester@ and its current status.
--
--
--
-- /See:/ 'suggesterStatus' smart constructor.
data SuggesterStatus = SuggesterStatus'
  { _ssOptions :: !Suggester,
    _ssStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuggesterStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssOptions' - Undocumented member.
--
-- * 'ssStatus' - Undocumented member.
suggesterStatus ::
  -- | 'ssOptions'
  Suggester ->
  -- | 'ssStatus'
  OptionStatus ->
  SuggesterStatus
suggesterStatus pOptions_ pStatus_ =
  SuggesterStatus' {_ssOptions = pOptions_, _ssStatus = pStatus_}

-- | Undocumented member.
ssOptions :: Lens' SuggesterStatus Suggester
ssOptions = lens _ssOptions (\s a -> s {_ssOptions = a})

-- | Undocumented member.
ssStatus :: Lens' SuggesterStatus OptionStatus
ssStatus = lens _ssStatus (\s a -> s {_ssStatus = a})

instance FromXML SuggesterStatus where
  parseXML x =
    SuggesterStatus' <$> (x .@ "Options") <*> (x .@ "Status")

instance Hashable SuggesterStatus

instance NFData SuggesterStatus
