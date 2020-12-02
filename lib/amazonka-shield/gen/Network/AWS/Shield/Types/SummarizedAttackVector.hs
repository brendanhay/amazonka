{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SummarizedAttackVector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedAttackVector where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.SummarizedCounter

-- | A summary of information about the attack.
--
--
--
-- /See:/ 'summarizedAttackVector' smart constructor.
data SummarizedAttackVector = SummarizedAttackVector'
  { _savVectorCounters ::
      !(Maybe [SummarizedCounter]),
    _savVectorType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SummarizedAttackVector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'savVectorCounters' - The list of counters that describe the details of the attack.
--
-- * 'savVectorType' - The attack type, for example, SNMP reflection or SYN flood.
summarizedAttackVector ::
  -- | 'savVectorType'
  Text ->
  SummarizedAttackVector
summarizedAttackVector pVectorType_ =
  SummarizedAttackVector'
    { _savVectorCounters = Nothing,
      _savVectorType = pVectorType_
    }

-- | The list of counters that describe the details of the attack.
savVectorCounters :: Lens' SummarizedAttackVector [SummarizedCounter]
savVectorCounters = lens _savVectorCounters (\s a -> s {_savVectorCounters = a}) . _Default . _Coerce

-- | The attack type, for example, SNMP reflection or SYN flood.
savVectorType :: Lens' SummarizedAttackVector Text
savVectorType = lens _savVectorType (\s a -> s {_savVectorType = a})

instance FromJSON SummarizedAttackVector where
  parseJSON =
    withObject
      "SummarizedAttackVector"
      ( \x ->
          SummarizedAttackVector'
            <$> (x .:? "VectorCounters" .!= mempty) <*> (x .: "VectorType")
      )

instance Hashable SummarizedAttackVector

instance NFData SummarizedAttackVector
