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
-- Module      : Network.AWS.RDS.DescribeValidDBInstanceModifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can call 'DescribeValidDBInstanceModifications' to learn what modifications you can make to your DB instance. You can use this information when you call 'ModifyDBInstance' .
--
--
module Network.AWS.RDS.DescribeValidDBInstanceModifications
    (
    -- * Creating a Request
      describeValidDBInstanceModifications
    , DescribeValidDBInstanceModifications
    -- * Request Lenses
    , dvdimDBInstanceIdentifier

    -- * Destructuring the Response
    , describeValidDBInstanceModificationsResponse
    , DescribeValidDBInstanceModificationsResponse
    -- * Response Lenses
    , dvdimrsValidDBInstanceModificationsMessage
    , dvdimrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeValidDBInstanceModifications' smart constructor.
newtype DescribeValidDBInstanceModifications = DescribeValidDBInstanceModifications'
  { _dvdimDBInstanceIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeValidDBInstanceModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvdimDBInstanceIdentifier' - The customer identifier or the ARN of your DB instance.
describeValidDBInstanceModifications
    :: Text -- ^ 'dvdimDBInstanceIdentifier'
    -> DescribeValidDBInstanceModifications
describeValidDBInstanceModifications pDBInstanceIdentifier_ =
  DescribeValidDBInstanceModifications'
    {_dvdimDBInstanceIdentifier = pDBInstanceIdentifier_}


-- | The customer identifier or the ARN of your DB instance.
dvdimDBInstanceIdentifier :: Lens' DescribeValidDBInstanceModifications Text
dvdimDBInstanceIdentifier = lens _dvdimDBInstanceIdentifier (\ s a -> s{_dvdimDBInstanceIdentifier = a})

instance AWSRequest
           DescribeValidDBInstanceModifications
         where
        type Rs DescribeValidDBInstanceModifications =
             DescribeValidDBInstanceModificationsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeValidDBInstanceModificationsResult"
              (\ s h x ->
                 DescribeValidDBInstanceModificationsResponse' <$>
                   (x .@? "ValidDBInstanceModificationsMessage") <*>
                     (pure (fromEnum s)))

instance Hashable
           DescribeValidDBInstanceModifications
         where

instance NFData DescribeValidDBInstanceModifications
         where

instance ToHeaders
           DescribeValidDBInstanceModifications
         where
        toHeaders = const mempty

instance ToPath DescribeValidDBInstanceModifications
         where
        toPath = const "/"

instance ToQuery DescribeValidDBInstanceModifications
         where
        toQuery DescribeValidDBInstanceModifications'{..}
          = mconcat
              ["Action" =:
                 ("DescribeValidDBInstanceModifications" ::
                    ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBInstanceIdentifier" =: _dvdimDBInstanceIdentifier]

-- | /See:/ 'describeValidDBInstanceModificationsResponse' smart constructor.
data DescribeValidDBInstanceModificationsResponse = DescribeValidDBInstanceModificationsResponse'
  { _dvdimrsValidDBInstanceModificationsMessage :: !(Maybe ValidDBInstanceModificationsMessage)
  , _dvdimrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeValidDBInstanceModificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvdimrsValidDBInstanceModificationsMessage' - Undocumented member.
--
-- * 'dvdimrsResponseStatus' - -- | The response status code.
describeValidDBInstanceModificationsResponse
    :: Int -- ^ 'dvdimrsResponseStatus'
    -> DescribeValidDBInstanceModificationsResponse
describeValidDBInstanceModificationsResponse pResponseStatus_ =
  DescribeValidDBInstanceModificationsResponse'
    { _dvdimrsValidDBInstanceModificationsMessage = Nothing
    , _dvdimrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dvdimrsValidDBInstanceModificationsMessage :: Lens' DescribeValidDBInstanceModificationsResponse (Maybe ValidDBInstanceModificationsMessage)
dvdimrsValidDBInstanceModificationsMessage = lens _dvdimrsValidDBInstanceModificationsMessage (\ s a -> s{_dvdimrsValidDBInstanceModificationsMessage = a})

-- | -- | The response status code.
dvdimrsResponseStatus :: Lens' DescribeValidDBInstanceModificationsResponse Int
dvdimrsResponseStatus = lens _dvdimrsResponseStatus (\ s a -> s{_dvdimrsResponseStatus = a})

instance NFData
           DescribeValidDBInstanceModificationsResponse
         where
