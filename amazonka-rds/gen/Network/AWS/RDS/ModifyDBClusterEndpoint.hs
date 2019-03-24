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
-- Module      : Network.AWS.RDS.ModifyDBClusterEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of an endpoint in an Amazon Aurora DB cluster.
--
--
module Network.AWS.RDS.ModifyDBClusterEndpoint
    (
    -- * Creating a Request
      modifyDBClusterEndpoint
    , ModifyDBClusterEndpoint
    -- * Request Lenses
    , mdceStaticMembers
    , mdceEndpointType
    , mdceExcludedMembers
    , mdceDBClusterEndpointIdentifier

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

-- | /See:/ 'modifyDBClusterEndpoint' smart constructor.
data ModifyDBClusterEndpoint = ModifyDBClusterEndpoint'
  { _mdceStaticMembers               :: !(Maybe [Text])
  , _mdceEndpointType                :: !(Maybe Text)
  , _mdceExcludedMembers             :: !(Maybe [Text])
  , _mdceDBClusterEndpointIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyDBClusterEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdceStaticMembers' - List of DB instance identifiers that are part of the custom endpoint group.
--
-- * 'mdceEndpointType' - The type of the endpoint. One of: @READER@ , @ANY@ .
--
-- * 'mdceExcludedMembers' - List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- * 'mdceDBClusterEndpointIdentifier' - The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
modifyDBClusterEndpoint
    :: Text -- ^ 'mdceDBClusterEndpointIdentifier'
    -> ModifyDBClusterEndpoint
modifyDBClusterEndpoint pDBClusterEndpointIdentifier_ =
  ModifyDBClusterEndpoint'
    { _mdceStaticMembers = Nothing
    , _mdceEndpointType = Nothing
    , _mdceExcludedMembers = Nothing
    , _mdceDBClusterEndpointIdentifier = pDBClusterEndpointIdentifier_
    }


-- | List of DB instance identifiers that are part of the custom endpoint group.
mdceStaticMembers :: Lens' ModifyDBClusterEndpoint [Text]
mdceStaticMembers = lens _mdceStaticMembers (\ s a -> s{_mdceStaticMembers = a}) . _Default . _Coerce

-- | The type of the endpoint. One of: @READER@ , @ANY@ .
mdceEndpointType :: Lens' ModifyDBClusterEndpoint (Maybe Text)
mdceEndpointType = lens _mdceEndpointType (\ s a -> s{_mdceEndpointType = a})

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
mdceExcludedMembers :: Lens' ModifyDBClusterEndpoint [Text]
mdceExcludedMembers = lens _mdceExcludedMembers (\ s a -> s{_mdceExcludedMembers = a}) . _Default . _Coerce

-- | The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
mdceDBClusterEndpointIdentifier :: Lens' ModifyDBClusterEndpoint Text
mdceDBClusterEndpointIdentifier = lens _mdceDBClusterEndpointIdentifier (\ s a -> s{_mdceDBClusterEndpointIdentifier = a})

instance AWSRequest ModifyDBClusterEndpoint where
        type Rs ModifyDBClusterEndpoint = DBClusterEndpoint
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyDBClusterEndpointResult"
              (\ s h x -> parseXML x)

instance Hashable ModifyDBClusterEndpoint where

instance NFData ModifyDBClusterEndpoint where

instance ToHeaders ModifyDBClusterEndpoint where
        toHeaders = const mempty

instance ToPath ModifyDBClusterEndpoint where
        toPath = const "/"

instance ToQuery ModifyDBClusterEndpoint where
        toQuery ModifyDBClusterEndpoint'{..}
          = mconcat
              ["Action" =:
                 ("ModifyDBClusterEndpoint" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "StaticMembers" =:
                 toQuery
                   (toQueryList "member" <$> _mdceStaticMembers),
               "EndpointType" =: _mdceEndpointType,
               "ExcludedMembers" =:
                 toQuery
                   (toQueryList "member" <$> _mdceExcludedMembers),
               "DBClusterEndpointIdentifier" =:
                 _mdceDBClusterEndpointIdentifier]
