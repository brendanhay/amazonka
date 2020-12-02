{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.MessageTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.MessageTag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the name and value of a tag that you can provide to @SendEmail@ or @SendRawEmail@ to apply to an email.
--
--
-- Message tags, which you use with configuration sets, enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'messageTag' smart constructor.
data MessageTag = MessageTag' {_mtName :: !Text, _mtValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtName' - The name of the tag. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
--
-- * 'mtValue' - The value of the tag. The value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
messageTag ::
  -- | 'mtName'
  Text ->
  -- | 'mtValue'
  Text ->
  MessageTag
messageTag pName_ pValue_ =
  MessageTag' {_mtName = pName_, _mtValue = pValue_}

-- | The name of the tag. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
mtName :: Lens' MessageTag Text
mtName = lens _mtName (\s a -> s {_mtName = a})

-- | The value of the tag. The value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
mtValue :: Lens' MessageTag Text
mtValue = lens _mtValue (\s a -> s {_mtValue = a})

instance Hashable MessageTag

instance NFData MessageTag

instance ToQuery MessageTag where
  toQuery MessageTag' {..} =
    mconcat ["Name" =: _mtName, "Value" =: _mtValue]
