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
-- Module      : Network.AWS.RDS.CopyDBClusterParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB cluster parameter group.
--
--
module Network.AWS.RDS.CopyDBClusterParameterGroup
    (
    -- * Creating a Request
      copyDBClusterParameterGroup
    , CopyDBClusterParameterGroup
    -- * Request Lenses
    , cdbcpgTags
    , cdbcpgSourceDBClusterParameterGroupIdentifier
    , cdbcpgTargetDBClusterParameterGroupIdentifier
    , cdbcpgTargetDBClusterParameterGroupDescription

    -- * Destructuring the Response
    , copyDBClusterParameterGroupResponse
    , CopyDBClusterParameterGroupResponse
    -- * Response Lenses
    , cdcpgrsDBClusterParameterGroup
    , cdcpgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'copyDBClusterParameterGroup' smart constructor.
data CopyDBClusterParameterGroup = CopyDBClusterParameterGroup'
  { _cdbcpgTags                                     :: !(Maybe [Tag])
  , _cdbcpgSourceDBClusterParameterGroupIdentifier  :: !Text
  , _cdbcpgTargetDBClusterParameterGroupIdentifier  :: !Text
  , _cdbcpgTargetDBClusterParameterGroupDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbcpgTags' - Undocumented member.
--
-- * 'cdbcpgSourceDBClusterParameterGroupIdentifier' - The identifier or Amazon Resource Name (ARN) for the source DB cluster parameter group. For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .  Constraints:     * Must specify a valid DB cluster parameter group.     * If the source DB cluster parameter group is in the same AWS Region as the copy, specify a valid DB parameter group identifier, for example @my-db-cluster-param-group@ , or a valid ARN.     * If the source DB parameter group is in a different AWS Region than the copy, specify a valid DB cluster parameter group ARN, for example @arn:aws:rds:us-east-1:123456789012:cluster-pg:custom-cluster-group1@ .
--
-- * 'cdbcpgTargetDBClusterParameterGroupIdentifier' - The identifier for the copied DB cluster parameter group. Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-cluster-param-group1@
--
-- * 'cdbcpgTargetDBClusterParameterGroupDescription' - A description for the copied DB cluster parameter group.
copyDBClusterParameterGroup
    :: Text -- ^ 'cdbcpgSourceDBClusterParameterGroupIdentifier'
    -> Text -- ^ 'cdbcpgTargetDBClusterParameterGroupIdentifier'
    -> Text -- ^ 'cdbcpgTargetDBClusterParameterGroupDescription'
    -> CopyDBClusterParameterGroup
copyDBClusterParameterGroup pSourceDBClusterParameterGroupIdentifier_ pTargetDBClusterParameterGroupIdentifier_ pTargetDBClusterParameterGroupDescription_ =
  CopyDBClusterParameterGroup'
    { _cdbcpgTags = Nothing
    , _cdbcpgSourceDBClusterParameterGroupIdentifier =
        pSourceDBClusterParameterGroupIdentifier_
    , _cdbcpgTargetDBClusterParameterGroupIdentifier =
        pTargetDBClusterParameterGroupIdentifier_
    , _cdbcpgTargetDBClusterParameterGroupDescription =
        pTargetDBClusterParameterGroupDescription_
    }


-- | Undocumented member.
cdbcpgTags :: Lens' CopyDBClusterParameterGroup [Tag]
cdbcpgTags = lens _cdbcpgTags (\ s a -> s{_cdbcpgTags = a}) . _Default . _Coerce

-- | The identifier or Amazon Resource Name (ARN) for the source DB cluster parameter group. For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .  Constraints:     * Must specify a valid DB cluster parameter group.     * If the source DB cluster parameter group is in the same AWS Region as the copy, specify a valid DB parameter group identifier, for example @my-db-cluster-param-group@ , or a valid ARN.     * If the source DB parameter group is in a different AWS Region than the copy, specify a valid DB cluster parameter group ARN, for example @arn:aws:rds:us-east-1:123456789012:cluster-pg:custom-cluster-group1@ .
cdbcpgSourceDBClusterParameterGroupIdentifier :: Lens' CopyDBClusterParameterGroup Text
cdbcpgSourceDBClusterParameterGroupIdentifier = lens _cdbcpgSourceDBClusterParameterGroupIdentifier (\ s a -> s{_cdbcpgSourceDBClusterParameterGroupIdentifier = a})

-- | The identifier for the copied DB cluster parameter group. Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-cluster-param-group1@
cdbcpgTargetDBClusterParameterGroupIdentifier :: Lens' CopyDBClusterParameterGroup Text
cdbcpgTargetDBClusterParameterGroupIdentifier = lens _cdbcpgTargetDBClusterParameterGroupIdentifier (\ s a -> s{_cdbcpgTargetDBClusterParameterGroupIdentifier = a})

-- | A description for the copied DB cluster parameter group.
cdbcpgTargetDBClusterParameterGroupDescription :: Lens' CopyDBClusterParameterGroup Text
cdbcpgTargetDBClusterParameterGroupDescription = lens _cdbcpgTargetDBClusterParameterGroupDescription (\ s a -> s{_cdbcpgTargetDBClusterParameterGroupDescription = a})

instance AWSRequest CopyDBClusterParameterGroup where
        type Rs CopyDBClusterParameterGroup =
             CopyDBClusterParameterGroupResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "CopyDBClusterParameterGroupResult"
              (\ s h x ->
                 CopyDBClusterParameterGroupResponse' <$>
                   (x .@? "DBClusterParameterGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CopyDBClusterParameterGroup where

instance NFData CopyDBClusterParameterGroup where

instance ToHeaders CopyDBClusterParameterGroup where
        toHeaders = const mempty

instance ToPath CopyDBClusterParameterGroup where
        toPath = const "/"

instance ToQuery CopyDBClusterParameterGroup where
        toQuery CopyDBClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("CopyDBClusterParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _cdbcpgTags),
               "SourceDBClusterParameterGroupIdentifier" =:
                 _cdbcpgSourceDBClusterParameterGroupIdentifier,
               "TargetDBClusterParameterGroupIdentifier" =:
                 _cdbcpgTargetDBClusterParameterGroupIdentifier,
               "TargetDBClusterParameterGroupDescription" =:
                 _cdbcpgTargetDBClusterParameterGroupDescription]

-- | /See:/ 'copyDBClusterParameterGroupResponse' smart constructor.
data CopyDBClusterParameterGroupResponse = CopyDBClusterParameterGroupResponse'
  { _cdcpgrsDBClusterParameterGroup :: !(Maybe DBClusterParameterGroup)
  , _cdcpgrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyDBClusterParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcpgrsDBClusterParameterGroup' - Undocumented member.
--
-- * 'cdcpgrsResponseStatus' - -- | The response status code.
copyDBClusterParameterGroupResponse
    :: Int -- ^ 'cdcpgrsResponseStatus'
    -> CopyDBClusterParameterGroupResponse
copyDBClusterParameterGroupResponse pResponseStatus_ =
  CopyDBClusterParameterGroupResponse'
    { _cdcpgrsDBClusterParameterGroup = Nothing
    , _cdcpgrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cdcpgrsDBClusterParameterGroup :: Lens' CopyDBClusterParameterGroupResponse (Maybe DBClusterParameterGroup)
cdcpgrsDBClusterParameterGroup = lens _cdcpgrsDBClusterParameterGroup (\ s a -> s{_cdcpgrsDBClusterParameterGroup = a})

-- | -- | The response status code.
cdcpgrsResponseStatus :: Lens' CopyDBClusterParameterGroupResponse Int
cdcpgrsResponseStatus = lens _cdcpgrsResponseStatus (\ s a -> s{_cdcpgrsResponseStatus = a})

instance NFData CopyDBClusterParameterGroupResponse
         where
