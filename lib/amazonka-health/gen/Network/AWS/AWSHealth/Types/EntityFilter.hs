{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityFilter where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EntityStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter> operation.
--
--
--
-- /See:/ 'entityFilter' smart constructor.
data EntityFilter = EntityFilter'
  { _eStatusCodes ::
      !(Maybe (List1 EntityStatusCode)),
    _eEntityARNs :: !(Maybe (List1 Text)),
    _eEntityValues :: !(Maybe (List1 Text)),
    _eTags :: !(Maybe [Map Text (Text)]),
    _eLastUpdatedTimes :: !(Maybe (List1 DateTimeRange)),
    _eEventARNs :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatusCodes' - A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
--
-- * 'eEntityARNs' - A list of entity ARNs (unique identifiers).
--
-- * 'eEntityValues' - A list of IDs for affected entities.
--
-- * 'eTags' - A map of entity tags attached to the affected entity.
--
-- * 'eLastUpdatedTimes' - A list of the most recent dates and times that the entity was updated.
--
-- * 'eEventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
entityFilter ::
  -- | 'eEventARNs'
  NonEmpty Text ->
  EntityFilter
entityFilter pEventARNs_ =
  EntityFilter'
    { _eStatusCodes = Nothing,
      _eEntityARNs = Nothing,
      _eEntityValues = Nothing,
      _eTags = Nothing,
      _eLastUpdatedTimes = Nothing,
      _eEventARNs = _List1 # pEventARNs_
    }

-- | A list of entity status codes (@IMPAIRED@ , @UNIMPAIRED@ , or @UNKNOWN@ ).
eStatusCodes :: Lens' EntityFilter (Maybe (NonEmpty EntityStatusCode))
eStatusCodes = lens _eStatusCodes (\s a -> s {_eStatusCodes = a}) . mapping _List1

-- | A list of entity ARNs (unique identifiers).
eEntityARNs :: Lens' EntityFilter (Maybe (NonEmpty Text))
eEntityARNs = lens _eEntityARNs (\s a -> s {_eEntityARNs = a}) . mapping _List1

-- | A list of IDs for affected entities.
eEntityValues :: Lens' EntityFilter (Maybe (NonEmpty Text))
eEntityValues = lens _eEntityValues (\s a -> s {_eEntityValues = a}) . mapping _List1

-- | A map of entity tags attached to the affected entity.
eTags :: Lens' EntityFilter [HashMap Text (Text)]
eTags = lens _eTags (\s a -> s {_eTags = a}) . _Default . _Coerce

-- | A list of the most recent dates and times that the entity was updated.
eLastUpdatedTimes :: Lens' EntityFilter (Maybe (NonEmpty DateTimeRange))
eLastUpdatedTimes = lens _eLastUpdatedTimes (\s a -> s {_eLastUpdatedTimes = a}) . mapping _List1

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
eEventARNs :: Lens' EntityFilter (NonEmpty Text)
eEventARNs = lens _eEventARNs (\s a -> s {_eEventARNs = a}) . _List1

instance Hashable EntityFilter

instance NFData EntityFilter

instance ToJSON EntityFilter where
  toJSON EntityFilter' {..} =
    object
      ( catMaybes
          [ ("statusCodes" .=) <$> _eStatusCodes,
            ("entityArns" .=) <$> _eEntityARNs,
            ("entityValues" .=) <$> _eEntityValues,
            ("tags" .=) <$> _eTags,
            ("lastUpdatedTimes" .=) <$> _eLastUpdatedTimes,
            Just ("eventArns" .= _eEventARNs)
          ]
      )
