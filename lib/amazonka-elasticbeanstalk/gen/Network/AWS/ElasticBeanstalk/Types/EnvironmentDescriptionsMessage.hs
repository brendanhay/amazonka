{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage where

import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Result message containing a list of environment descriptions.
--
--
--
-- /See:/ 'environmentDescriptionsMessage' smart constructor.
data EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
  { _edmNextToken ::
      !(Maybe Text),
    _edmEnvironments ::
      !( Maybe
           [EnvironmentDescription]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentDescriptionsMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edmNextToken' - In a paginated request, the token that you can pass in a subsequent request to get the next response page.
--
-- * 'edmEnvironments' - Returns an 'EnvironmentDescription' list.
environmentDescriptionsMessage ::
  EnvironmentDescriptionsMessage
environmentDescriptionsMessage =
  EnvironmentDescriptionsMessage'
    { _edmNextToken = Nothing,
      _edmEnvironments = Nothing
    }

-- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
edmNextToken :: Lens' EnvironmentDescriptionsMessage (Maybe Text)
edmNextToken = lens _edmNextToken (\s a -> s {_edmNextToken = a})

-- | Returns an 'EnvironmentDescription' list.
edmEnvironments :: Lens' EnvironmentDescriptionsMessage [EnvironmentDescription]
edmEnvironments = lens _edmEnvironments (\s a -> s {_edmEnvironments = a}) . _Default . _Coerce

instance FromXML EnvironmentDescriptionsMessage where
  parseXML x =
    EnvironmentDescriptionsMessage'
      <$> (x .@? "NextToken")
      <*> (x .@? "Environments" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable EnvironmentDescriptionsMessage

instance NFData EnvironmentDescriptionsMessage
