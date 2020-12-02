{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PartialMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PartialMatch where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The reference rule that partially matches the @ViolationTarget@ rule and violation reason.
--
--
--
-- /See:/ 'partialMatch' smart constructor.
data PartialMatch = PartialMatch'
  { _pmTargetViolationReasons ::
      !(Maybe [Text]),
    _pmReference :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartialMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmTargetViolationReasons' - The violation reason.
--
-- * 'pmReference' - The reference rule from the master security group of the AWS Firewall Manager policy.
partialMatch ::
  PartialMatch
partialMatch =
  PartialMatch'
    { _pmTargetViolationReasons = Nothing,
      _pmReference = Nothing
    }

-- | The violation reason.
pmTargetViolationReasons :: Lens' PartialMatch [Text]
pmTargetViolationReasons = lens _pmTargetViolationReasons (\s a -> s {_pmTargetViolationReasons = a}) . _Default . _Coerce

-- | The reference rule from the master security group of the AWS Firewall Manager policy.
pmReference :: Lens' PartialMatch (Maybe Text)
pmReference = lens _pmReference (\s a -> s {_pmReference = a})

instance FromJSON PartialMatch where
  parseJSON =
    withObject
      "PartialMatch"
      ( \x ->
          PartialMatch'
            <$> (x .:? "TargetViolationReasons" .!= mempty)
            <*> (x .:? "Reference")
      )

instance Hashable PartialMatch

instance NFData PartialMatch
