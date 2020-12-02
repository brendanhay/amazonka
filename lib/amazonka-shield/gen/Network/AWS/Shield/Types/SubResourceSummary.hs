{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubResourceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SubResourceSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.SubResourceType
import Network.AWS.Shield.Types.SummarizedAttackVector
import Network.AWS.Shield.Types.SummarizedCounter

-- | The attack information for the specified SubResource.
--
--
--
-- /See:/ 'subResourceSummary' smart constructor.
data SubResourceSummary = SubResourceSummary'
  { _srsCounters ::
      !(Maybe [SummarizedCounter]),
    _srsAttackVectors ::
      !(Maybe [SummarizedAttackVector]),
    _srsId :: !(Maybe Text),
    _srsType :: !(Maybe SubResourceType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubResourceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsCounters' - The counters that describe the details of the attack.
--
-- * 'srsAttackVectors' - The list of attack types and associated counters.
--
-- * 'srsId' - The unique identifier (ID) of the @SubResource@ .
--
-- * 'srsType' - The @SubResource@ type.
subResourceSummary ::
  SubResourceSummary
subResourceSummary =
  SubResourceSummary'
    { _srsCounters = Nothing,
      _srsAttackVectors = Nothing,
      _srsId = Nothing,
      _srsType = Nothing
    }

-- | The counters that describe the details of the attack.
srsCounters :: Lens' SubResourceSummary [SummarizedCounter]
srsCounters = lens _srsCounters (\s a -> s {_srsCounters = a}) . _Default . _Coerce

-- | The list of attack types and associated counters.
srsAttackVectors :: Lens' SubResourceSummary [SummarizedAttackVector]
srsAttackVectors = lens _srsAttackVectors (\s a -> s {_srsAttackVectors = a}) . _Default . _Coerce

-- | The unique identifier (ID) of the @SubResource@ .
srsId :: Lens' SubResourceSummary (Maybe Text)
srsId = lens _srsId (\s a -> s {_srsId = a})

-- | The @SubResource@ type.
srsType :: Lens' SubResourceSummary (Maybe SubResourceType)
srsType = lens _srsType (\s a -> s {_srsType = a})

instance FromJSON SubResourceSummary where
  parseJSON =
    withObject
      "SubResourceSummary"
      ( \x ->
          SubResourceSummary'
            <$> (x .:? "Counters" .!= mempty)
            <*> (x .:? "AttackVectors" .!= mempty)
            <*> (x .:? "Id")
            <*> (x .:? "Type")
      )

instance Hashable SubResourceSummary

instance NFData SubResourceSummary
