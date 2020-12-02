{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResolvedTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResolvedTargets where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about targets that resolved during the Automation execution.
--
--
--
-- /See:/ 'resolvedTargets' smart constructor.
data ResolvedTargets = ResolvedTargets'
  { _rtTruncated ::
      !(Maybe Bool),
    _rtParameterValues :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResolvedTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtTruncated' - A boolean value indicating whether the resolved target list is truncated.
--
-- * 'rtParameterValues' - A list of parameter values sent to targets that resolved during the Automation execution.
resolvedTargets ::
  ResolvedTargets
resolvedTargets =
  ResolvedTargets'
    { _rtTruncated = Nothing,
      _rtParameterValues = Nothing
    }

-- | A boolean value indicating whether the resolved target list is truncated.
rtTruncated :: Lens' ResolvedTargets (Maybe Bool)
rtTruncated = lens _rtTruncated (\s a -> s {_rtTruncated = a})

-- | A list of parameter values sent to targets that resolved during the Automation execution.
rtParameterValues :: Lens' ResolvedTargets [Text]
rtParameterValues = lens _rtParameterValues (\s a -> s {_rtParameterValues = a}) . _Default . _Coerce

instance FromJSON ResolvedTargets where
  parseJSON =
    withObject
      "ResolvedTargets"
      ( \x ->
          ResolvedTargets'
            <$> (x .:? "Truncated") <*> (x .:? "ParameterValues" .!= mempty)
      )

instance Hashable ResolvedTargets

instance NFData ResolvedTargets
