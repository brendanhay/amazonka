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
-- Module      : Network.AWS.Shield.DescribeAttack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a DDoS attack.
--
--
module Network.AWS.Shield.DescribeAttack
    (
    -- * Creating a Request
      describeAttack
    , DescribeAttack
    -- * Request Lenses
    , daAttackId

    -- * Destructuring the Response
    , describeAttackResponse
    , DescribeAttackResponse
    -- * Response Lenses
    , darsAttack
    , darsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'describeAttack' smart constructor.
newtype DescribeAttack = DescribeAttack'
  { _daAttackId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAttack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAttackId' - The unique identifier (ID) for the attack that to be described.
describeAttack
    :: Text -- ^ 'daAttackId'
    -> DescribeAttack
describeAttack pAttackId_ = DescribeAttack' {_daAttackId = pAttackId_}


-- | The unique identifier (ID) for the attack that to be described.
daAttackId :: Lens' DescribeAttack Text
daAttackId = lens _daAttackId (\ s a -> s{_daAttackId = a})

instance AWSRequest DescribeAttack where
        type Rs DescribeAttack = DescribeAttackResponse
        request = postJSON shield
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAttackResponse' <$>
                   (x .?> "Attack") <*> (pure (fromEnum s)))

instance Hashable DescribeAttack where

instance NFData DescribeAttack where

instance ToHeaders DescribeAttack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.DescribeAttack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAttack where
        toJSON DescribeAttack'{..}
          = object
              (catMaybes [Just ("AttackId" .= _daAttackId)])

instance ToPath DescribeAttack where
        toPath = const "/"

instance ToQuery DescribeAttack where
        toQuery = const mempty

-- | /See:/ 'describeAttackResponse' smart constructor.
data DescribeAttackResponse = DescribeAttackResponse'
  { _darsAttack         :: !(Maybe AttackDetail)
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAttackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAttack' - The attack that is described.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAttackResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAttackResponse
describeAttackResponse pResponseStatus_ =
  DescribeAttackResponse'
    {_darsAttack = Nothing, _darsResponseStatus = pResponseStatus_}


-- | The attack that is described.
darsAttack :: Lens' DescribeAttackResponse (Maybe AttackDetail)
darsAttack = lens _darsAttack (\ s a -> s{_darsAttack = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAttackResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAttackResponse where
