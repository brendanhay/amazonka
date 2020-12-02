{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRuleRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRuleRecord where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.SamplingRule

-- | A 'SamplingRule' and its metadata.
--
--
--
-- /See:/ 'samplingRuleRecord' smart constructor.
data SamplingRuleRecord = SamplingRuleRecord'
  { _srrModifiedAt ::
      !(Maybe POSIX),
    _srrSamplingRule :: !(Maybe SamplingRule),
    _srrCreatedAt :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SamplingRuleRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrModifiedAt' - When the rule was last modified.
--
-- * 'srrSamplingRule' - The sampling rule.
--
-- * 'srrCreatedAt' - When the rule was created.
samplingRuleRecord ::
  SamplingRuleRecord
samplingRuleRecord =
  SamplingRuleRecord'
    { _srrModifiedAt = Nothing,
      _srrSamplingRule = Nothing,
      _srrCreatedAt = Nothing
    }

-- | When the rule was last modified.
srrModifiedAt :: Lens' SamplingRuleRecord (Maybe UTCTime)
srrModifiedAt = lens _srrModifiedAt (\s a -> s {_srrModifiedAt = a}) . mapping _Time

-- | The sampling rule.
srrSamplingRule :: Lens' SamplingRuleRecord (Maybe SamplingRule)
srrSamplingRule = lens _srrSamplingRule (\s a -> s {_srrSamplingRule = a})

-- | When the rule was created.
srrCreatedAt :: Lens' SamplingRuleRecord (Maybe UTCTime)
srrCreatedAt = lens _srrCreatedAt (\s a -> s {_srrCreatedAt = a}) . mapping _Time

instance FromJSON SamplingRuleRecord where
  parseJSON =
    withObject
      "SamplingRuleRecord"
      ( \x ->
          SamplingRuleRecord'
            <$> (x .:? "ModifiedAt")
            <*> (x .:? "SamplingRule")
            <*> (x .:? "CreatedAt")
      )

instance Hashable SamplingRuleRecord

instance NFData SamplingRuleRecord
