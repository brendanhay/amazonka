{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreateTopic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic to which notifications can be published. Users can create at most 100,000 topics. For more information, see <http://aws.amazon.com/sns/ http://aws.amazon.com/sns> . This action is idempotent, so if the requester already owns a topic with the specified name, that topic's ARN is returned without creating a new topic.
--
--
module Network.AWS.SNS.CreateTopic
    (
    -- * Creating a Request
      createTopic
    , CreateTopic
    -- * Request Lenses
    , ctAttributes
    , ctName

    -- * Destructuring the Response
    , createTopicResponse
    , CreateTopicResponse
    -- * Response Lenses
    , ctrsTopicARN
    , ctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for CreateTopic action.
--
--
--
-- /See:/ 'createTopic' smart constructor.
data CreateTopic = CreateTopic'
  { _ctAttributes :: !(Maybe (Map Text Text))
  , _ctName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctAttributes' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @CreateTopic@ action uses:     * @DeliveryPolicy@
