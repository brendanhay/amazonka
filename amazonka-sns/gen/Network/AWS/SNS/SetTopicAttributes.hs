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
-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a topic owner to set an attribute of the topic to a new value.
--
--
module Network.AWS.SNS.SetTopicAttributes
    (
    -- * Creating a Request
      setTopicAttributes
    , SetTopicAttributes
    -- * Request Lenses
    , staAttributeValue
    , staTopicARN
    , staAttributeName

    -- * Destructuring the Response
    , setTopicAttributesResponse
    , SetTopicAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for SetTopicAttributes action.
--
--
--
-- /See:/ 'setTopicAttributes' smart constructor.
data SetTopicAttributes = SetTopicAttributes'
  { _staAttributeValue :: !(Maybe Text)
  , _staTopicARN       :: !Text
  , _staAttributeName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetTopicAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staAttributeValue' - The new value for the attribute.
--
-- * 'staTopicARN' - The ARN of the topic to modify.
--
-- * 'staAttributeName' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:     * @DeliveryPolicy@
