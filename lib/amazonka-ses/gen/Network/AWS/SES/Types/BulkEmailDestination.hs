{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BulkEmailDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.Destination
import Network.AWS.SES.Types.MessageTag

-- | An array that contains one or more Destinations, as well as the tags and replacement data associated with each of those Destinations.
--
--
--
-- /See:/ 'bulkEmailDestination' smart constructor.
data BulkEmailDestination = BulkEmailDestination'
  { _bedReplacementTemplateData ::
      !(Maybe Text),
    _bedReplacementTags :: !(Maybe [MessageTag]),
    _bedDestination :: !Destination
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BulkEmailDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bedReplacementTemplateData' - A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- * 'bedReplacementTags' - A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- * 'bedDestination' - Undocumented member.
bulkEmailDestination ::
  -- | 'bedDestination'
  Destination ->
  BulkEmailDestination
bulkEmailDestination pDestination_ =
  BulkEmailDestination'
    { _bedReplacementTemplateData = Nothing,
      _bedReplacementTags = Nothing,
      _bedDestination = pDestination_
    }

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
bedReplacementTemplateData :: Lens' BulkEmailDestination (Maybe Text)
bedReplacementTemplateData = lens _bedReplacementTemplateData (\s a -> s {_bedReplacementTemplateData = a})

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
bedReplacementTags :: Lens' BulkEmailDestination [MessageTag]
bedReplacementTags = lens _bedReplacementTags (\s a -> s {_bedReplacementTags = a}) . _Default . _Coerce

-- | Undocumented member.
bedDestination :: Lens' BulkEmailDestination Destination
bedDestination = lens _bedDestination (\s a -> s {_bedDestination = a})

instance Hashable BulkEmailDestination

instance NFData BulkEmailDestination

instance ToQuery BulkEmailDestination where
  toQuery BulkEmailDestination' {..} =
    mconcat
      [ "ReplacementTemplateData" =: _bedReplacementTemplateData,
        "ReplacementTags"
          =: toQuery (toQueryList "member" <$> _bedReplacementTags),
        "Destination" =: _bedDestination
      ]
