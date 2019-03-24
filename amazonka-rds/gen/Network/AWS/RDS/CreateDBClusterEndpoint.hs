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
-- Module      : Network.AWS.RDS.CreateDBClusterEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom endpoint and associates it with an Amazon Aurora DB cluster.
--
--
module Network.AWS.RDS.CreateDBClusterEndpoint
    (
    -- * Creating a Request
      createDBClusterEndpoint
    , CreateDBClusterEndpoint
    -- * Request Lenses
    , cdceStaticMembers
    , cdceExcludedMembers
    , cdceDBClusterIdentifier
    , cdceDBClusterEndpointIdentifier
    , cdceEndpointType

    -- * Destructuring the Response
    , dbClusterEndpoint
    , DBClusterEndpoint
    -- * Response Lenses
    , dceStatus
    , dceDBClusterIdentifier
    , dceDBClusterEndpointARN
    , dceCustomEndpointType
    , dceStaticMembers
    , dceEndpointType
    , dceDBClusterEndpointIdentifier
    , dceEndpoint
    , dceDBClusterEndpointResourceIdentifier
    , dceExcludedMembers
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDBClusterEndpoint' smart constructor.
data CreateDBClusterEndpoint = CreateDBClusterEndpoint'
  { _cdceStaticMembers               :: !(Maybe [Text])
  , _cdceExcludedMembers             :: !(Maybe [Text])
  , _cdceDBClusterIdentifier         :: !Text
  , _cdceDBClusterEndpointIdentifier :: !Text
  , _cdceEndpointType                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBClusterEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdceStaticMembers' - List of DB instance identifiers that are part of the custom endpoint group.
--
-- * 'cdceExcludedMembers' - List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- * 'cdceDBClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- * 'cdceDBClusterEndpointIdentifier' - The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
--
-- * 'cdceEndpointType' - The type of the endpoint. One of: @READER@ , @ANY@ .
createDBClusterEndpoint
    :: Text -- ^ 'cdceDBClusterIdentifier'
    -> Text -- ^ 'cdceDBClusterEndpointIdentifier'
    -> Text -- ^ 'cdceEndpointType'
    -> CreateDBClusterEndpoint
createDBClusterEndpoint pDBClusterIdentifier_ pDBClusterEndpointIdentifier_ pEndpointType_ =
  CreateDBClusterEndpoint'
    { _cdceStaticMembers = Nothing
    , _cdceExcludedMembers = Nothing
    , _cdceDBClusterIdentifier = pDBClusterIdentifier_
    , _cdceDBClusterEndpointIdentifier = pDBClusterEndpointIdentifier_
    , _cdceEndpointType = pEndpointType_
    }


-- | List of DB instance identifiers that are part of the custom endpoint group.
cdceStaticMembers :: Lens' CreateDBClusterEndpoint [Text]
cdceStaticMembers = lens _cdceStaticMembers (\ s a -> s{_cdceStaticMembers = a}) . _Default . _Coerce

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
cdceExcludedMembers :: Lens' CreateDBClusterEndpoint [Text]
cdceExcludedMembers = lens _cdceExcludedMembers (\ s a -> s{_cdceExcludedMembers = a}) . _Default . _Coerce

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
cdceDBClusterIdentifier :: Lens' CreateDBClusterEndpoint Text
cdceDBClusterIdentifier = lens _cdceDBClusterIdentifier (\ s a -> s{_cdceDBClusterIdentifier = a})

-- | The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
cdceDBClusterEndpointIdentifier :: Lens' CreateDBClusterEndpoint Text
cdceDBClusterEndpointIdentifier = lens _cdceDBClusterEndpointIdentifier (\ s a -> s{_cdceDBClusterEndpointIdentifier = a})

-- | The type of the endpoint. One of: @READER@ , @ANY@ .
cdceEndpointType :: Lens' CreateDBClusterEndpoint Text
cdceEndpointType = lens _cdceEndpointType (\ s a -> s{_cdceEndpointType = a})

instance AWSRequest CreateDBClusterEndpoint where
        type Rs CreateDBClusterEndpoint = DBClusterEndpoint
        request = postQuery rds
        response
          = receiveXMLWrapper "CreateDBClusterEndpointResult"
              (\ s h x -> parseXML x)

instance Hashable CreateDBClusterEndpoint where

instance NFData CreateDBClusterEndpoint where

instance ToHeaders CreateDBClusterEndpoint where
        toHeaders = const mempty

instance ToPath CreateDBClusterEndpoint where
        toPath = const "/"

instance ToQuery CreateDBClusterEndpoint where
        toQuery CreateDBClusterEndpoint'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBClusterEndpoint" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "StaticMembers" =:
                 toQuery
                   (toQueryList "member" <$> _cdceStaticMembers),
               "ExcludedMembers" =:
                 toQuery
                   (toQueryList "member" <$> _cdceExcludedMembers),
               "DBClusterIdentifier" =: _cdceDBClusterIdentifier,
               "DBClusterEndpointIdentifier" =:
                 _cdceDBClusterEndpointIdentifier,
               "EndpointType" =: _cdceEndpointType]
